(ns status-im.ui.screens.wallet.send.views
  (:require-macros [status-im.utils.views :refer [defview letsubs]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]
            [status-im.i18n :as i18n]
            [status-im.ui.components.react :as react]
            [status-im.ui.components.styles :as components.styles]
            [status-im.ui.components.toolbar.view :as topbar]
            [status-im.ui.components.toolbar :as toolbar]
            [status-im.ui.screens.wallet.components.views :as wallet.components]
            [status-im.ui.screens.wallet.send.styles :as styles]))

(defn- sign-transaction-button [sign-enabled?]
  [toolbar/toolbar
   {:right {:type                :next
            :disabled?           (not sign-enabled?)
            :on-press            #(re-frame/dispatch [:wallet.ui/sign-transaction-button-clicked])
            :accessibility-label :sign-transaction-button
            :label               :t/transactions-sign-transaction}}])

;;TODO DEPRECATED
(defn- render-send-transaction-view
  [{:keys [transaction scroll amount-input]}]
  (let [{:keys [from amount amount-text amount-error token sign-enabled?
                asset-error to to-name symbol]} transaction]
    [wallet.components/simple-screen {:avoid-keyboard? true}
     [topbar/simple-toolbar (i18n/label :t/send-transaction)]
     [react/view components.styles/flex
      [wallet.components/network-info]
      [react/scroll-view {:keyboard-should-persist-taps :never
                          :keyboard-dismiss-mode        :on-drag
                          :ref                          #(reset! scroll %)
                          :on-content-size-change       #(when (and scroll @scroll)
                                                           (.scrollToEnd @scroll))}
       [react/view styles/send-transaction-form
        [wallet.components/recipient-selector
         {:address   to
          :name      to-name}]
        [wallet.components/asset-selector
         {:error     asset-error
          :address   from
          :type      :send
          :symbol    symbol}]
        [wallet.components/amount-selector
         {:error         amount-error
          :amount        amount
          :amount-text   amount-text
          :input-options {:on-change-text #(re-frame/dispatch [:wallet.send/set-and-validate-amount %])
                          :ref            (partial reset! amount-input)}} token]]]
      [sign-transaction-button sign-enabled?]]]))

(defn- send-transaction-view [{:keys [scroll]}]
  (let [amount-input (atom nil)
        handler #(when (and scroll @scroll @amount-input (.isFocused @amount-input)) (.scrollToEnd @scroll))]
    (reagent/create-class
     {:component-did-mount (fn [_]
                             ;;NOTE(goranjovic): keyboardDidShow is for android and keyboardWillShow for ios
                             (.addListener react/keyboard "keyboardDidShow" handler)
                             (.addListener react/keyboard "keyboardWillShow" handler))
      :reagent-render       (fn [opts] (render-send-transaction-view
                                        (assoc opts :amount-input amount-input)))})))

(defview send-transaction []
  (letsubs [transaction [:wallet.send/transaction]
            scroll (atom nil)]
    [send-transaction-view {:transaction    transaction
                            :scroll         scroll}]))
