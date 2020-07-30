# Getting Started (Mac OSX)

## Workstation Setup

- Install [Docker](https://docs.docker.com/docker-for-mac/install/)
- Install the [Haskell Platform](https://www.haskell.org/platform/mac.html)
    - GHC
    - Cabal build system
    - Stack tool
    
## Build and Run the consortium locally 

### Build

- Git clone this project
    ```bash
    git clone git@github.com:dolla-consortium/consortium.git
    ```
-  Go at the root of the clone :
    ```bash
    stack build
    ```
### Run

You are about to run a local consortium of 10 teams all plugged on 1 instance of the event store behind.

1) Run the [eventStore](https://eventstore.org/) on docker

    ```bash
    docker run --name eventstore-service -dit -p 2113:2113 -p 1113:1113 eventstore/eventstore -e EVENTSTORE_DISABLE_HTTP_CACHING=True
    ```
    This command `docker run` will :
    - [x] Download the [EventStore](https://eventstore.org/) docker image
       > *Warning* : This operation could take a while since you need to download the docker image...
    - [x] Run that docker image into a docker container named `eventstore-service` (in background mode)
    - [x] Open the docker container on the port 1113 for dolla services
    - [x] Open the docker container on the port 2113 for the [event store website](http://localhost:2113/web/index.html#/dashboard) (username: `admin`, password: `changeit` )

    Tips for handling that docker container created :
    - `docker stop eventstore-service` to stop the service.
    - `docker start eventstore-service` to start the service.
    - `docker rm -f eventstore-service` to delete the container.

2) run the local consortium
- Run and see the log of one specific node (recommend way)
```bash
stack run dolla-local-consortium 2>&1 >/dev/null | grep '\[dolla.team-07174aef-4842-4b2f-93cf-31e6c333bcf7\]'
```
- Run and see all the logs for each node
```bash
stack run dolla-local-consortium
```
3) Feed the consortium with requests
At this point you should have a a local consortium running but with no clients communicating with it. You can use the client simulator to propagate requests in the consortium 
```bash
stack run dolla-cli-client-simulator
```