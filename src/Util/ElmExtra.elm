module Util.ElmExtra exposing (..) 

import Task
import Process
import Time

sendAfter : Time.Time -> msg -> Cmd msg
sendAfter t c =
  Process.sleep t 
  |> Task.perform (always c)
