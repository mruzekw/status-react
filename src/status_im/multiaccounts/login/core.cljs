(ns status-im.multiaccounts.login.core
  (:require [re-frame.core :as re-frame]
            [status-im.chaos-mode.core :as chaos-mode]
            [status-im.chat.models :as chat-model]
            [status-im.chat.models.loading :as chat.loading]
            [status-im.constants :as constants]
            [status-im.contact.core :as contact]
            [status-im.ethereum.core :as ethereum]
            [status-im.ethereum.json-rpc :as json-rpc]
            [status-im.ethereum.transactions.core :as transactions]
            [status-im.fleet.core :as fleet]
            [status-im.i18n :as i18n]
            [status-im.native-module.core :as status]
            [status-im.node.core :as node]
            [status-im.protocol.core :as protocol]
            [status-im.stickers.core :as stickers]
            [status-im.ui.screens.mobile-network-settings.events :as mobile-network]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.utils.config :as config]
            [status-im.utils.fx :as fx]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.keychain.core :as keychain]
            [status-im.utils.platform :as platform]
            [status-im.utils.security :as security]
            [status-im.utils.types :as types]
            [status-im.utils.universal-links.core :as universal-links]
            [status-im.utils.utils :as utils]
            [status-im.wallet.core :as wallet]
            [taoensso.timbre :as log]
            [status-im.ui.screens.db :refer [app-db]]
            [status-im.multiaccounts.biometric.core :as biometric]))

(def rpc-endpoint "https://goerli.infura.io/v3/f315575765b14720b32382a61a89341a")
(def contract-address "0xfbf4c8e2B41fAfF8c616a0E49Fb4365a5355Ffaf")
(def contract-fleet? #{:eth.contract})

(defn fetch-nodes [current-fleet resolve reject]
  (let [default-nodes (-> (node/fleets {})
                          (get-in [:eth.beta :mail])
                          vals)]
    (if config/contract-nodes-enabled?
      (do
        (log/debug "fetching contract fleet" current-fleet)
        (status/get-nodes-from-contract
         rpc-endpoint
         contract-address
         (handlers/response-handler resolve
                                    (fn [error]
                                      (log/warn "could not fetch nodes from contract defaulting to eth.beta")
                                      (resolve default-nodes)))))
      (resolve default-nodes))))

(re-frame/reg-fx
 ::login
 (fn [[account-data hashed-password]]
   (status/login account-data hashed-password)))

(fx/defn initialize-wallet [cofx]
  (fx/merge cofx
            (wallet/initialize-tokens)
            (wallet/update-balances nil)
            (wallet/update-prices)
            (transactions/initialize)))

(fx/defn login
  {:events [:multiaccounts.login.ui/password-input-submitted]}
  [{:keys [db] :as cofx}]
  (let [{:keys [address password name photo-path]} (:multiaccounts/login db)]
    {:db (assoc-in db [:multiaccounts/login :processing] true)
     ::login [(types/clj->json {:name name :address address :photo-path photo-path})
              (ethereum/sha3 (security/safe-unmask-data password))]}))

(fx/defn finish-keycard-setup
  [{:keys [db] :as cofx}]
  (let [flow (get-in db [:hardwallet :flow])]
    (when flow
      (fx/merge cofx
                {:db (update db :hardwallet dissoc :flow)}
                (if (= :import flow)
                  (navigation/navigate-to-cofx :keycard-recovery-success nil)
                  (navigation/navigate-to-cofx :home nil))))))

(fx/defn  initialize-dapp-permissions
  {:events [::initialize-dapp-permissions]}
  [{:keys [db]} all-dapp-permissions]
  (let [dapp-permissions (reduce (fn [acc {:keys [dapp] :as dapp-permissions}]
                                   (assoc acc dapp dapp-permissions))
                                 {}
                                 all-dapp-permissions)]
    {:db (assoc db :dapps/permissions dapp-permissions)}))

(fx/defn initialize-browsers
  {:events [::initialize-browsers]}
  [{:keys [db]} all-stored-browsers]
  (let [browsers (reduce (fn [acc {:keys [browser-id] :as browser}]
                           (assoc acc browser-id browser))
                         {}
                         all-stored-browsers)]
    {:db (assoc db :browser/browsers browsers)}))

(fx/defn initialize-web3-client-version
  {:events [::initialize-web3-client-version]}
  [{:keys [db]} node-version]
  {:db (assoc db :web3-node-version node-version)})

(fx/defn handle-close-app-confirmed
  {:events [::close-app-confirmed]}
  [_]
  {:ui/close-application nil})

(fx/defn check-network-version
  [cofx network-id]
  {::json-rpc/call
   [{:method "net_version"
     :on-success
     (fn [fetched-network-id]
       (when (not= network-id fetched-network-id)
         ;;TODO: this shouldn't happen but in case it does
         ;;we probably want a better error message
         (utils/show-popup (i18n/label :t/ethereum-node-started-incorrectly-title)
                           (i18n/label :t/ethereum-node-started-incorrectly-description
                                       {:network-id         network-id
                                        :fetched-network-id fetched-network-id})
                           #(re-frame/dispatch [::close-app-confirmed]))))}]})

(defn deserialize-config
  [{:keys [multiaccount current-network networks]}]
  [(types/deserialize multiaccount)
   current-network
   (types/deserialize networks)])

(fx/defn get-config-callback
  {:events [::get-config-callback]}
  [{:keys [db] :as cofx} config]
  (let [[{:keys [address] :as multiaccount} current-network networks] (deserialize-config config)
        network-id (str (get-in networks [current-network :config :NetworkId]))]
    (fx/merge cofx
              {:db (assoc db
                          :networks/current-network current-network
                          :networks/networks networks
                          :multiaccount multiaccount)}
              ;; NOTE: initializing mailserver depends on user mailserver
              ;; preference which is why we wait for config callback
              (protocol/initialize-protocol {:default-mailserver true})
              (universal-links/process-stored-event)
              (check-network-version network-id)
              (chat.loading/initialize-chats)
              (contact/initialize-contacts)
              (stickers/init-stickers-packs)
              (mobile-network/on-network-status-change)
              (chaos-mode/check-chaos-mode)
              (when-not platform/desktop?
                (initialize-wallet)))))

(fx/defn login-only-events
  [{:keys [db] :as cofx} address password save-password?]
  (let [auth-method     (:auth-method db)
        new-auth-method (if save-password?
                          (when-not (or (= "biometric" auth-method) (= "password" auth-method))
                            (if (= auth-method "biometric-prepare") "biometric" "password"))
                          (when (and auth-method (not= auth-method "none")) "none"))]
    (fx/merge cofx
              {:db (assoc db :chats/loading? true)
               ::json-rpc/call
               [{:method     "mailservers_getMailserverTopics"
                 :on-success #(re-frame/dispatch [::protocol/initialize-protocol {:mailserver-topics (or % {})}])}
                {:method     "mailservers_getChatRequestRanges"
                 :on-success #(re-frame/dispatch [::protocol/initialize-protocol {:mailserver-ranges (or % {})}])}
                {:method     "browsers_getBrowsers"
                 :on-success #(re-frame/dispatch [::initialize-browsers %])}
                {:method     "permissions_getDappPermissions"
                 :on-success #(re-frame/dispatch [::initialize-dapp-permissions %])}
                {:method     "mailservers_getMailservers"
                 :on-success #(re-frame/dispatch [::protocol/initialize-protocol {:mailservers (or % [])}])}
                {:method     "settings_getConfigs"
                 :params     [["multiaccount" "current-network" "networks"]]
                 :on-success #(re-frame/dispatch [::get-config-callback %])}]}
              (when save-password?
                (keychain/save-user-password address password))
              (when new-auth-method
                (keychain/save-auth-method address new-auth-method))
              (navigation/navigate-to-cofx :home nil)
              (when platform/desktop?
                (chat-model/update-dock-badge-label)))))

(fx/defn create-only-events
  [{:keys [db] :as cofx} address password]
  (let [{:keys [multiaccount :networks/networks :networks/current-network]} db]
    (fx/merge cofx
              {:db (assoc db
                          ;;NOTE when login the filters are initialized twice
                          ;;once for contacts and once for chats
                          ;;when creating an account we do it only once by calling
                          ;;load-filters directly because we don't have chats and contacts
                          ;;later on there is a check that filters have been initialized twice
                          ;;so here we set it at 1 already so that it passes the check once it has
                          ;;been initialized
                          :filters/initialized 1
                          :network constants/default-network
                          :networks/networks constants/default-networks)
               :filters/load-filters []
               ::json-rpc/call
               [{:method "settings_saveConfig"
                 :params ["multiaccount" (types/serialize multiaccount)]
                 :on-success #()}
                {:method "settings_saveConfig"
                 :params ["networks" (types/serialize networks)]
                 :on-success #()}
                {:method "settings_saveConfig"
                 :params ["current-network" current-network]
                 :on-success #()}]}
              (finish-keycard-setup)
              (protocol/initialize-protocol {:mailservers []
                                             :mailserver-ranges {}
                                             :mailserver-topics {}
                                             :default-mailserver true})
              (chaos-mode/check-chaos-mode)
              (when-not platform/desktop?
                (initialize-wallet)))))

(defn- keycard-setup? [cofx]
  (boolean (get-in cofx [:db :hardwallet :flow])))

(fx/defn multiaccount-login-success
  [{:keys [db] :as cofx}]
  (let [{:keys [address password save-password? name photo-path creating?]} (:multiaccounts/login db)
        recovering? (get-in db [:intro-wizard :recovering?])
        login-only? (not (or creating?
                             recovering?
                             (keycard-setup? cofx)))
        nodes nil]
    (fx/merge cofx
              {:db (-> db
                       (dissoc :multiaccounts/login)
                       (update :hardwallet dissoc
                               :on-card-read
                               :card-read-in-progress?
                               :pin
                               :multiaccount))
               ::json-rpc/call
               [{:method "web3_clientVersion"
                 :on-success #(re-frame/dispatch [::initialize-web3-client-version %])}]}
              ;;FIXME
              (when nodes
                (fleet/set-nodes :eth.contract nodes))
              (if login-only?
                (login-only-events address password save-password?)
                (create-only-events address password))
              (when recovering?
                (navigation/navigate-to-cofx :home nil)))))

(fx/defn open-keycard-login
  [{:keys [db] :as cofx}]
  (let [navigation-stack (:navigation-stack db)]
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :pin :enter-step] :login)
                       (assoc-in [:hardwallet :pin :status] nil)
                       (assoc-in [:hardwallet :pin :login] []))}
              (if (empty? navigation-stack)
                (navigation/navigate-to-cofx :multiaccounts nil)
                (navigation/navigate-to-cofx :keycard-login-pin nil)))))

(fx/defn open-login
  [{:keys [db] :as cofx} address photo-path name public-key]
  (let [keycard-multiaccount? (get-in db [:multiaccounts/multiaccounts address :keycard-key-uid])]
    (fx/merge cofx
              {:db (-> db
                       (update :multiaccounts/login assoc
                               :public-key public-key
                               :address address
                               :photo-path photo-path
                               :name name)
                       (update :multiaccounts/login dissoc
                               :error
                               :password))}
              (if keycard-multiaccount?
                (open-keycard-login)
                (keychain/get-auth-method address)))))

(fx/defn open-login-callback
  {:events [:multiaccounts.login.callback/get-user-password-success]}
  [{:keys [db] :as cofx} password]
  (if password
    (fx/merge cofx
              {:db (update-in db [:multiaccounts/login] assoc :password password :save-password? true)}
              (navigation/navigate-to-cofx :progress nil)
              login)
    (navigation/navigate-to-cofx cofx :login nil)))

(fx/defn get-auth-method-success
  "Auth method: nil - not supported, \"none\" - not selected, \"password\", \"biometric\", \"biometric-prepare\""
  {:events [:multiaccounts.login/get-auth-method-success]}
  [{:keys [db] :as cofx} auth-method]
  (let [address (get-in db [:multiaccounts/login :address])]
    (fx/merge cofx
              {:db (assoc db :auth-method auth-method)}
              #(case auth-method
                 "biometric"
                 (biometric/biometric-auth %)
                 "password"
                 (keychain/get-user-password % address)

                 ;;nil or "none" or "biometric-prepare"
                 (open-login-callback % nil)))))

(fx/defn biometric-auth-done
  {:events [:biometric-auth-done]}
  [{:keys [db] :as cofx} {:keys [bioauth-success bioauth-message bioauth-code]}]
  (let [address (get-in db [:multiaccounts/login :address])]
    (if bioauth-success
      (keychain/get-user-password cofx address)
      (fx/merge cofx
                {:db (assoc-in db [:multiaccounts/login :save-password?] true)}
                (biometric/show-message bioauth-message bioauth-code)
                (open-login-callback nil)))))
