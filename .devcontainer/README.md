# Summary:

Starting at the root of this repo, run the following to get a local Marlowe playground
going on `localhost:8009`:
```
$(nix-build -A marlowe-playground.server-invoker)/bin/marlowe-playground webserver
$(nix-build -A marlowe-playground.server-invoker)/bin/marlowe-playground psgenerator ./marlowe-playground-client/generated
cd marlowe-playground-client
yarn
yarn purs:compile ## THIS COMMAND WILL FAIL BUT THAT'S OKAY KEEP GOING
yarn run webpack:server
```

Then, to run Marlowe Shiny:
```
cd marlowe-actus
cabal run marlowe-shiny --with-compiler=/root/.ghcup/bin/ghc-8.10.1
```

## How to get the local Marlowe playground going

From the root of this repo, run:
```
$(nix-build -A marlowe-playground.server-invoker)/bin/marlowe-playground webserver
```

That will start the backend webserver. Then start a new terminal or shell session
and run:

```
$(nix-build -A marlowe-playground.server-invoker)/bin/marlowe-playground psgenerator ./marlowe-playground-client/generated
```

That will generate some purescript files needed for the front end. Now change
directory into the `marlowe-playground-client` directory:
```
cd marlowe-playground-client
```

Now run `yarn` to download/install any missing javascript dependencies.
```
yarn
```

Now run the following command to install purescript dependencies. **WARNING**:
This command will fail, but that is okay because it does what we need it to do
before it fails.
```
yarn purs:compile
```

Finally, run the following for an auto-reloading dev build on `localhost:8009`:
```
yarn run webpack:server
```

# Summary:
```