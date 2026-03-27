# Stage 1: Build
FROM haskell:9.8-slim AS builder

WORKDIR /app

# Copy cabal files first for dependency caching
COPY proof-chain.cabal cabal.project ./
RUN cabal update && cabal build --only-dependencies

# Copy source and build
COPY app/ app/
COPY src/ src/

RUN cabal build exe:proof-chain && \
    cp $(cabal list-bin proof-chain) /app/proof-chain-bin

# Stage 2: Runtime
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      libgmp10 \
      netbase && \
    rm -rf /var/lib/apt/lists/* && \
    groupadd -r appuser && \
    useradd -r -g appuser -d /home/appuser -m appuser

WORKDIR /home/appuser

COPY --from=builder /app/proof-chain-bin ./proof-chain

RUN chown appuser:appuser ./proof-chain && \
    chmod +x ./proof-chain

USER appuser

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD ["./proof-chain", "--help"] || exit 1

ENTRYPOINT ["./proof-chain"]
