(ns status-im.ui.screens.fleet-settings.views
  (:require [re-frame.core :as re-frame]
            [status-im.i18n :as i18n]
            [status-im.ui.components.icons.vector-icons :as vector-icons]
            [status-im.ui.components.list.views :as list]
            [status-im.ui.components.react :as react]
            [status-im.ui.components.status-bar.view :as status-bar]
            [status-im.ui.components.toolbar.view :as toolbar]
            [status-im.ui.screens.fleet-settings.styles :as styles]
            [status-im.node.core :as node]
            [status-im.utils.platform :as platform])
  (:require-macros [status-im.utils.views :as views]))

(defn- fleet-icon [current?]
  [react/view (if platform/desktop?
                {:style (styles/fleet-icon-container current?)}
                (styles/fleet-icon-container current?))
   [vector-icons/icon :main-icons/mailserver
    (if platform/desktop? {:style (styles/fleet-icon current?)}
        (styles/fleet-icon current?))]])

(defn change-fleet [fleet]
  (re-frame/dispatch [:fleet.ui/fleet-selected fleet]))

(defn render-row [current-fleet]
  (fn [fleet]
    (let [current? (= fleet current-fleet)]
      [react/touchable-highlight
       {:on-press #(change-fleet fleet)
        :accessibility-label :fleet-item}
       [react/view styles/fleet-item
        [fleet-icon current?]
        [react/view styles/fleet-item-inner
         [react/text {:style styles/fleet-item-name-text}
          fleet]]]])))

(defn fleets [custom-fleets]
  (map name (keys (node/fleets {:custom-fleets custom-fleets}))))

(views/defview fleet-settings []
  (views/letsubs [custom-fleets [:fleets/custom-fleets]
                  current-fleet [:settings/current-fleet]]
    [react/view {:flex 1}
     [status-bar/status-bar]
     [toolbar/toolbar {}
      toolbar/default-nav-back
      [toolbar/content-title (i18n/label :t/fleet-settings)]]
     [react/view styles/wrapper
      [list/flat-list {:data               (fleets custom-fleets)
                       :default-separator? false
                       :key-fn             identity
                       :render-fn          (render-row (name current-fleet))}]]]))
