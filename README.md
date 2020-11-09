/ [Consensus](https://github.com/dolla-consortium/consensus) / [Proposing](#proposing-section)
# Proposing Section

- [Section Overview](#section-overview)
- [DevOps](#devops)
- [Development Tasks (Mac OSX)](#development-tasks-mac-osx)


# Section Overview

"Proposing" means
- **Receptioning** requests from customers and consortium members
- **Staging** these requests packaged into local proposals for being consumed by the consensus.

Each of these local proposals once accepted by the consortium will be uniformly transacted on each consortium node.
The transactions will then be appended into each node ledgers.

A **Section** is a meaningful Set of Pipelines and Junctions put together.

![proposing-overview](documentation/media/proposing-overview.png)

- [Receptioning](/packages/receptioning) : Client/Server for collecting requests.
- [Detecting Flow Tension](/packages/detecting-tension) : Detect if the local proposal flow is tensed, meaning if the consensus has consumed more local proposals than being staged.
- [Staging](/packages/staging) : Stage Local Proposal Files for being consumed by the consensus. Local Proposals have the following properties
  - `Proposals are never empty`
  - `Proposals file size < configurable size limit`
  - Filename - `x.proposal` with `x` the offset of proposal produced
- [Simulating](/packages/simulating) : Simulate the proposing input streams
  - Send dummy requests to `Receptioning` and simulate the downstream local proposal consumption.
  - 2 available modes
    - **Overflowing** : Sending more requests than the consortium can consume
    - **UnderSupplying** : Sending less request than the consortium can consume

# DevOps

[Zeus](/packages/zeus) : Local Deployment Tool
- Running only the proposing section in a simulated and local environment.
- Configuring Microservices via an interactive CLI :
  - Mode of simulation (Overflowing/UnderSupplying)
  - Proposal size limit

# Development Tasks (Mac OSX)

- [Environment Setup](documentation/development-task.md#environment-setup)
- [Run the tests](documentation/development-task.md#run-the-tests)
- [Install the executables](documentation/development-task.md#install-the-executables)
- [Running a Simulated Consensus Proposing](documentation/development-task.md#running-a-simulated-consensus-proposing)