#!/bin/bash

# SSL Certificate Testing Script
# Tests various aspects of the SSL certificate setup

CERT_DIR="./certs"
CERT_FILE="$CERT_DIR/localhost.crt"
KEY_FILE="$CERT_DIR/localhost.key"
HTTPS_PORT=8443
HTTP_PORT=8080

echo "SSL Certificate Testing Utility"
echo "==============================="

# Check if certificates exist
echo ""
echo "1. Checking certificate files..."
if [ -f "$CERT_FILE" ]; then
    echo "✓ Certificate file exists: $CERT_FILE"
else
    echo "✗ Certificate file missing: $CERT_FILE"
    echo "  Run ./generate_certs.sh to create certificates"
    exit 1
fi

if [ -f "$KEY_FILE" ]; then
    echo "✓ Private key file exists: $KEY_FILE"
else
    echo "✗ Private key file missing: $KEY_FILE"
    echo "  Run ./generate_certs.sh to create certificates"
    exit 1
fi

# Check certificate details
echo ""
echo "2. Certificate details..."
echo "Subject:"
openssl x509 -in "$CERT_FILE" -noout -subject | sed 's/subject=/  /'
echo "Issuer:"
openssl x509 -in "$CERT_FILE" -noout -issuer | sed 's/issuer=/  /'
echo "Validity:"
openssl x509 -in "$CERT_FILE" -noout -dates | sed 's/^/  /'
echo "Subject Alternative Names:"
openssl x509 -in "$CERT_FILE" -noout -text | grep -A 1 "Subject Alternative Name" | tail -n 1 | sed 's/^/  /'

# Check if certificate and key match
echo ""
echo "3. Validating certificate and key match..."
CERT_MODULUS=$(openssl x509 -in "$CERT_FILE" -noout -modulus | openssl md5)
KEY_MODULUS=$(openssl rsa -in "$KEY_FILE" -noout -modulus 2>/dev/null | openssl md5)

if [ "$CERT_MODULUS" = "$KEY_MODULUS" ]; then
    echo "✓ Certificate and private key match"
else
    echo "✗ Certificate and private key do not match"
    echo "  Certificate modulus: $CERT_MODULUS"
    echo "  Key modulus: $KEY_MODULUS"
fi

# Test certificate validity period
echo ""
echo "4. Checking certificate expiration..."
if openssl x509 -in "$CERT_FILE" -checkend 86400 -noout; then
    echo "✓ Certificate is valid and will not expire in the next day"
else
    echo "✗ Certificate will expire within 24 hours or has already expired"
fi

if openssl x509 -in "$CERT_FILE" -checkend 2592000 -noout; then
    echo "✓ Certificate will not expire in the next 30 days"
else
    echo "⚠ Certificate will expire within 30 days"
fi

# Test if web server is running and responding
echo ""
echo "5. Testing web server connectivity..."

# Test HTTP
if curl -s -o /dev/null -w "%{http_code}" http://localhost:$HTTP_PORT/ | grep -q "200\|404"; then
    echo "✓ HTTP server responding on port $HTTP_PORT"
else
    echo "✗ HTTP server not responding on port $HTTP_PORT"
fi

# Test HTTPS with insecure flag (since it's self-signed)
if curl -k -s -o /dev/null -w "%{http_code}" https://localhost:$HTTPS_PORT/ | grep -q "200\|404"; then
    echo "✓ HTTPS server responding on port $HTTPS_PORT"
else
    echo "✗ HTTPS server not responding on port $HTTPS_PORT"
    echo "  Make sure you started the server with: ./start_web_ssl.sh"
fi

# Test certificate from the server
echo ""
echo "6. Testing SSL/TLS connection..."
if echo | timeout 5 openssl s_client -connect localhost:$HTTPS_PORT -servername localhost 2>/dev/null | grep -q "Verify return code: 18 (self.signed certificate)"; then
    echo "✓ SSL/TLS connection successful (self-signed certificate as expected)"
elif echo | timeout 5 openssl s_client -connect localhost:$HTTPS_PORT -servername localhost 2>/dev/null | grep -q "Verify return code: 0 (ok)"; then
    echo "✓ SSL/TLS connection successful (trusted certificate)"
else
    echo "✗ SSL/TLS connection failed or certificate issues detected"
    echo "  Testing SSL connection details:"
    echo | timeout 5 openssl s_client -connect localhost:$HTTPS_PORT -servername localhost 2>&1 | grep -E "(Verify|Certificate chain|Server certificate)"
fi

# Show browser instructions
echo ""
echo "7. Browser Testing Instructions:"
echo "================================"
echo "To test HTTPS in your browser:"
echo "1. Open https://localhost:$HTTPS_PORT"
echo "2. Your browser will show a security warning (expected for self-signed certificates)"
echo "3. Click 'Advanced' and 'Proceed to localhost (unsafe)' or similar"
echo "4. Alternatively, add the certificate to your browser's trusted certificates:"
echo "   - Chrome/Safari: Import $CERT_FILE into System Keychain (macOS) or Certificate Store (Windows)"
echo "   - Firefox: Go to Settings > Privacy & Security > Certificates > View Certificates > Import"
echo ""
echo "API Testing:"
echo "HTTP:  curl http://localhost:$HTTP_PORT/api/system/health"
echo "HTTPS: curl -k https://localhost:$HTTPS_PORT/api/system/health"
echo "SSL Info: curl -k https://localhost:$HTTPS_PORT/api/ssl/info"
echo "SSL Validation: curl -k https://localhost:$HTTPS_PORT/api/ssl/validate"