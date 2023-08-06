# Stack resolver 18.18 uses ghc 8.10.7
FROM haskell:8.10.7 as build
RUN mkdir -p /sahasrara/build
WORKDIR /sahasrara/build

# System lib dependencies
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3-dev build-essential pkg-config libicu-dev --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY . .

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /sahasrara/build/bin

FROM haskell:8.10.7-slim as app

# System runtime dependencies
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libicu63 --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /sahasrara
WORKDIR /sahasrara

COPY --from=build /sahasrara/build/bin .
# apparently we need the .git folder
COPY .git .git
# also we need the runtime resources folder
COPY resources resources

# Run the bot
CMD /sahasrara/sahasrara-exe
