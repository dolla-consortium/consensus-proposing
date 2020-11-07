/ [Consensus](https://github.com/dolla-consortium/consensus) / [Proposing](https://github.com/dolla-consortium/consensus-proposing) / Detecting Starvation
# Detecting Starvation


# Overview

 ![overview](documentation/media/overview.png)

`Packaging` is a simple ***Pipeline***
- a persisted input stream : [Input.hs](lib/Dolla/Consensus/Proposing/Packaging/Pipeline/IO/Input.hs)
- a line of Pipes Welded together in [Pipeline.hs](lib/Dolla/Consensus/Proposing/Packaging/Pipeline/Pipeline.hs)
  - One Pipe :
  - Welding : Adapting IOs between pipes

  ```
  serializing .~> nonEmptying .~> capping .~> persisting 
  ```
- a persisted output stream : [Output.hs](lib/Dolla/Consensus/Proposing/Packaging/Pipeline/IO/Output.hs)