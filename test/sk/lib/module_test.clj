(ns sk.lib.module-test
  (:require [clojure.test :as t] 
            [sk.lib.module :as m]))

;;
;; Module construction tests
;;

(def components
  {:info {:config {:name :dbname
                   :password :dbpass
                   :host :dbhost}}
   :store {:config #{:table}
           :deps {:database :info}}
   :service {:deps #{:store}}
   :endpoint {:make (fn [props]
                      {:name (:name props)
                       :provider (:service props)})
              :config {:name :endpoint-name}
              :deps #{:service}}})

(def config
  {:dbname "name"
   :dbhost "host"
   :dbpass "pass"
   :endpoint-name "e0"
   :table "commands"})

(def info-expected
  {:name "name"
   :password "pass"
   :host "host"})

(t/deftest module-test
  (let [def {:a {:deps #{:b}}
             :b {:deps #{:c}}}]
    (t/is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^Missing dependencies$"
                            (m/activation-order def))
          "Missing dependencies are detected"))

  (let [def {:a {:config {:url :a-url}}}
        thrown (atom false)]
    (try
      (m/build def {})
      (catch clojure.lang.ExceptionInfo e
        (reset! thrown true)
        (t/is (= (ex-data e)
                 {:component :a, :missing [:a-url]})
              "Missing property names are reported"))   
      (finally
        (t/is @thrown
              "Exception was thrown for missing name"))))
  
  (let [def {:a {:deps #{:b}}
             :b {:deps #{:c}}
             :c {:deps #{:a}}}]
    (t/is (= (m/read-cycle [:http]
                           {:store #{:http}
                            :process #{:store}
                            :api #{:store}
                            :http #{:api}})
             [:http :api :store :http]))
    (t/is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^Circular dependencies$"
                            (m/activation-order def))
          "Circular dependencies are prevented"))

  (t/is (= [:info :store :service :endpoint]
           (m/activation-order components))
        "Dependency activation order is calculated.")

  (t/is (= identity
           (m/component-constructor (:info components)))
        "a minimal component is equivalent to its configuration")

  (t/is (= info-expected
           (m/component-properties :key {} (:info components) config))
        "component property map construction")

  (t/is (= info-expected
           (m/build-component :key {} (:info components) config))
        "minimal component construction")

  (let [sys {:components {:service "service"}}]
    (t/is (= {:name "e0"
              :provider "service"}
             (m/build-component sys :endpoint (:endpoint components)
                                config))
          "nested component construction"))
  
  (t/is (= {:components {:endpoint
                         {:name "e0"
                          :provider {:store {:table "commands"
                                             :database info-expected}}}
                         :service {:store {:table "commands"
                                           :database info-expected}}
                         :store {:table "commands" :database info-expected}
                         :info info-expected}
            :order [:info :store :service :endpoint]}
           (m/build components config))
        "build module")

  (t/is (= {:components {:endpoint
                         {:name "e0"
                          :provider {:store {:table "commands"
                                             :database info-expected}}}
                         :service {:store {:table "commands"
                                           :database info-expected}}
                         :store {:table "commands" :database info-expected}
                         :info info-expected}
            :order [:info :store :service :endpoint]}
           (m/build (update components :store assoc :props
                            {:table "commands"})
                    (dissoc config :table)))
        "property literals"))

(defrecord Adder [])

(defn -add [_ {:keys [x y]}]
  (+ x y))

(m/defmodule Adder
  {:add -add})

(defrecord State [state])

(defn make-state [_]
  (->State (atom nil)))

(defn -start [{:keys [state]} _]
  (reset! state 3))

(defn -stop [{:keys [state]} _]
  (reset! state nil))

(defn -value [{:keys [state]} _]
  (deref state))

(m/defmodule State
  {:start -start
   :stop -stop
   :value -value})

(t/deftest assembly-test
  (m/with-module [m
                  {:adder {:make map->Adder}
                   :state {:make make-state}}
                  {}]

    (let [adder (m/-get m :adder)]
      (t/is (= 3 (m/-send adder :add {:x 1 :y 2})))

      (t/is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"^Module is null$"
             (m/-send nil :mul {:x 1 :y 2}))
            "")
      
      (t/is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"^Instance is not a Module$"
             (m/-send 3 :mul {:x 1 :y 2}))
            "")
      
      (t/is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"^Unknown message: class sk.lib.module_test.Adder/:mul$"
             (m/-send adder :mul {:x 1 :y 2}))
            ""))
    
    (let [state (m/-get m :state)]
      (t/is (= 3 (m/-send state :value {}))))

    (t/is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"^Unknown component ':unknown'$"
           (m/-send m :get :unknown)
          ""))))
    


    
                  
  
