/ [Consensus](https://github.com/dolla-consortium/consensus) / [Proposing](https://github.com/dolla-consortium/consensus-proposing) / [Receptioning](#receptioning)
# Receptioning

![overview](documentation/media/overview.png)

This non-deterministic pipeline
- Send Requests over the network via [Client.hs](lib/Dolla/Consensus/Proposing/Receptioning/API/Client/Client.hs)
- Receive Request via [Server.hs](lib/Dolla/Consensus/Proposing/Receptioning/API/Server/Server.hs)
- Append Non Idempotently Request directly into the Packaging Input Stream


