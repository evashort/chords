module AudioTime exposing ( now )

import Native.AudioTime

import Task exposing (Task)

now : Task x Float
now =
  Native.AudioTime.now
