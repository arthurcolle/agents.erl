#!/bin/bash

# SSL Certificate Generation Script for Development Testing
# This script generates self-signed certificates for local testing

CERT_DIR="./certs"
CERT_NAME="localhost"
KEY_FILE="$CERT_DIR/$CERT_NAME.key"
CERT_FILE="$CERT_DIR/$CERT_NAME.crt"
PEM_FILE="$CERT_DIR/$CERT_NAME.pem"

# Ensure certificate directory exists
mkdir -p "$CERT_DIR"

echo "Generating SSL certificates for development testing..."
echo "Certificate directory: $CERT_DIR"

# Generate private key
echo "Generating private key..."
openssl genrsa -out "$KEY_FILE" 2048

# Generate certificate signing request (CSR) with subject alternative names
echo "Generating certificate signing request..."
cat > "$CERT_DIR/cert.conf" << EOF
[req]
default_bits = 2048
prompt = no
default_md = sha256
distinguished_name = dn
req_extensions = v3_req

[dn]
C=US
ST=CA
L=San Francisco
O=Agents.erl Development
OU=Development Team
CN=localhost

[v3_req]
basicConstraints = CA:FALSE
keyUsage = nonRepudiation, digitalSignature, keyEncipherment
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
DNS.2 = *.localhost
DNS.3 = 127.0.0.1
IP.1 = 127.0.0.1
IP.2 = ::1
EOF

# Generate self-signed certificate
echo "Generating self-signed certificate..."
openssl req -new -x509 -key "$KEY_FILE" -out "$CERT_FILE" -days 365 -config "$CERT_DIR/cert.conf" -extensions v3_req

# Create combined PEM file (certificate + private key)
echo "Creating combined PEM file..."
cat "$CERT_FILE" "$KEY_FILE" > "$PEM_FILE"

# Set appropriate permissions
chmod 600 "$KEY_FILE"
chmod 644 "$CERT_FILE"
chmod 600 "$PEM_FILE"

echo "SSL certificates generated successfully!"
echo "Private key: $KEY_FILE"
echo "Certificate: $CERT_FILE"
echo "Combined PEM: $PEM_FILE"
echo ""
echo "To trust this certificate in your browser:"
echo "1. Open $CERT_FILE"
echo "2. Add it to your system's trusted certificates"
echo "3. Restart your browser"
echo ""
echo "Certificate details:"
openssl x509 -in "$CERT_FILE" -text -noout | grep -A 2 "Subject:\|Issuer:\|Not Before:\|Not After:\|DNS:"

# Clean up temporary config file
rm "$CERT_DIR/cert.conf"