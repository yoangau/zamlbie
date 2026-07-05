# Multi-stage build for zamlbie server
# Stage 1: Build the application
FROM ocaml/opam:debian-12-ocaml-5.3 AS builder

# Set working directory
WORKDIR /app

# Copy opam files first for better caching
COPY --chown=opam:opam zamlbie.opam dune-project ./

# Install system dependencies and opam dependencies
RUN sudo apt-get update && \
    sudo apt-get install -y pkg-config libgmp-dev && \
    opam update && \
    opam install -y --deps-only .

# Copy source code
COPY --chown=opam:opam . .

# Build the project
RUN eval $(opam env) && dune build bin/main_server.exe --release

# Stage 2: Runtime image
FROM debian:12-slim

# Install runtime dependencies (the server itself is pure OCaml; certs are
# only needed if it ever makes outbound TLS connections)
RUN apt-get update && \
    apt-get install -y ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 zamlbie

# Set working directory
WORKDIR /app

# Copy the built executable from builder stage
COPY --from=builder /app/_build/default/bin/main_server.exe /app/server

# Change ownership
RUN chown -R zamlbie:zamlbie /app

# Switch to non-root user
USER zamlbie

# Expose port (default 7777, can be overridden by ZAMLBIE_SERVER_PORT env var)
EXPOSE 7777

# Environment variables with defaults (can be overridden by Render.com)
ENV ZAMLBIE_SERVER_PORT=7777
ENV ZAMLBIE_SERVER_INTERFACE=0.0.0.0

# Run the server
CMD ["/app/server"]
