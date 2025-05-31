# SSL/TLS Certificate Testing Setup

This document provides comprehensive guidance for testing the web application with SSL/TLS certificates.

## Quick Start

1. **Generate certificates** (if not already done):
   ```bash
   ./generate_certs.sh
   ```

2. **Start the web server with SSL support**:
   ```bash
   ./start_web_ssl.sh
   ```

3. **Test the certificate setup**:
   ```bash
   ./test_certs.sh
   ```

4. **Access the application**:
   - HTTP: http://localhost:8080
   - HTTPS: https://localhost:8443

## Certificate Management

### Certificate Generation

The `generate_certs.sh` script creates self-signed certificates suitable for development testing:

- **Certificate file**: `./certs/localhost.crt`
- **Private key file**: `./certs/localhost.key`  
- **Combined PEM file**: `./certs/localhost.pem`
- **Validity**: 365 days from generation
- **Subject Alternative Names**: localhost, *.localhost, 127.0.0.1, ::1

### Certificate Details

The generated certificate includes:
- RSA 2048-bit key
- SHA-256 signature algorithm
- Subject: `CN=localhost, O=Agents.erl Development, OU=Development Team, L=San Francisco, ST=CA, C=US`
- Multiple DNS names and IP addresses for local testing

### Certificate Validation

Use the testing script to validate your certificate setup:

```bash
./test_certs.sh
```

This script checks:
- ✅ Certificate and key file existence
- ✅ Certificate format validation
- ✅ Private key format validation
- ✅ Certificate/key pair matching
- ✅ Certificate expiration status
- ✅ Web server connectivity
- ✅ SSL/TLS handshake

## Web Server Configuration

### SSL Configuration

The web server supports both HTTP and HTTPS simultaneously:

- **HTTP Port**: 8080 (always enabled)
- **HTTPS Port**: 8443 (enabled when SSL is configured)
- **SSL Protocols**: TLS 1.2, TLS 1.3
- **Cipher Suites**: Modern, secure ciphers only

### Configuration Files

SSL settings are controlled by `config/ssl.config`:

```erlang
[
  {agent_web, [
    {port, 8080},
    {https_port, 8443},
    {ssl_enabled, true},
    {cert_file, "./certs/localhost.crt"},
    {key_file, "./certs/localhost.key"}
  ]}
].
```

### Startup Scripts

- **HTTP only**: `./start_web.sh`
- **HTTP + HTTPS**: `./start_web_ssl.sh`

## API Endpoints

### System Health API

Check system status via both HTTP and HTTPS:

```bash
# HTTP
curl http://localhost:8080/api/system/health

# HTTPS (with self-signed certificate)
curl -k https://localhost:8443/api/system/health
```

### SSL Certificate API

The application provides APIs for certificate management:

#### Certificate Information
```bash
curl -k https://localhost:8443/api/ssl/info
```

Returns:
```json
{
  "ssl_enabled": true,
  "https_port": 8443,
  "cert_file": "./certs/localhost.crt",
  "key_file": "./certs/localhost.key",
  "cert_exists": true,
  "key_exists": true,
  "certificate_info": {
    "subject": "CN=localhost, O=Agents.erl Development, ...",
    "issuer": "CN=localhost, O=Agents.erl Development, ...",
    "not_before": 1748640241,
    "not_after": 1780176241,
    "version": 3
  }
}
```

#### Certificate Validation
```bash
curl -k https://localhost:8443/api/ssl/validate
```

Returns:
```json
{
  "valid": true,
  "error_count": 0,
  "results": [
    {
      "type": "certificate",
      "check": "file_exists",
      "valid": true,
      "file": "./certs/localhost.crt"
    },
    {
      "type": "private_key", 
      "check": "file_exists",
      "valid": true,
      "file": "./certs/localhost.key"
    },
    // ... more validation results
  ],
  "timestamp": "2025-05-30T21:24:01Z"
}
```

## Browser Testing

### Self-Signed Certificate Warnings

When testing with browsers, you'll encounter security warnings because the certificates are self-signed. This is expected behavior.

#### Chrome/Safari
1. Navigate to https://localhost:8443
2. Click "Advanced" on the security warning
3. Click "Proceed to localhost (unsafe)"

#### Firefox
1. Navigate to https://localhost:8443
2. Click "Advanced" on the security warning
3. Click "Accept the Risk and Continue"

### Installing Certificate as Trusted

For a better testing experience, you can install the certificate as trusted:

#### macOS
```bash
# Add to system keychain
sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain ./certs/localhost.crt
```

#### Windows
1. Double-click `certs/localhost.crt`
2. Click "Install Certificate"
3. Select "Local Machine" and "Place all certificates in the following store"
4. Choose "Trusted Root Certification Authorities"

#### Linux
```bash
# Ubuntu/Debian
sudo cp ./certs/localhost.crt /usr/local/share/ca-certificates/localhost.crt
sudo update-ca-certificates
```

## Development Testing Scenarios

### Basic HTTPS Testing
```bash
# Start server with SSL
./start_web_ssl.sh

# In another terminal, test endpoints
curl -k https://localhost:8443/api/system/health
curl -k https://localhost:8443/api/ssl/info
curl -k https://localhost:8443/api/ssl/validate
```

### WebSocket over SSL (WSS)
```javascript
// Connect to secure WebSocket
const ws = new WebSocket('wss://localhost:8443/ws');
ws.onopen = () => console.log('Secure WebSocket connected');
```

### MCP over HTTPS
All MCP endpoints work over HTTPS:
```bash
curl -k https://localhost:8443/api/mcp/servers
curl -k https://localhost:8443/api/mcp/status
```

### Certificate Rotation Testing
```bash
# Generate new certificates
./generate_certs.sh

# Restart server to pick up new certificates
# (Stop with Ctrl+C, then restart)
./start_web_ssl.sh

# Validate new certificates
./test_certs.sh
```

## Troubleshooting

### Certificate Issues

**Problem**: "Certificate and private key do not match"
```bash
# Check certificate modulus
openssl x509 -in ./certs/localhost.crt -noout -modulus | openssl md5
# Check key modulus  
openssl rsa -in ./certs/localhost.key -noout -modulus | openssl md5
# They should match
```

**Problem**: "Certificate has expired"
```bash
# Check expiration
openssl x509 -in ./certs/localhost.crt -noout -dates
# Regenerate if expired
./generate_certs.sh
```

### Connection Issues

**Problem**: "HTTPS server not responding"
1. Check if server started with SSL enabled
2. Verify port 8443 is not blocked by firewall
3. Check server logs for SSL initialization errors

**Problem**: "SSL handshake failed"
1. Verify certificate files exist and are readable
2. Check certificate validity
3. Ensure compatible SSL/TLS versions

### Server Configuration Issues

**Problem**: SSL not starting due to missing files
- The server will automatically fall back to HTTP-only if certificates are missing
- Check logs for certificate file paths
- Ensure certificate files have correct permissions (644 for cert, 600 for key)

## Security Considerations

### Development vs Production

**Development (current setup)**:
- Self-signed certificates
- Permissive SSL settings
- Certificate warnings expected

**Production recommendations**:
- Use certificates from trusted CA (Let's Encrypt, commercial CA)
- Implement HSTS headers
- Use certificate pinning where appropriate
- Regular certificate rotation
- Monitor certificate expiration

### Certificate Management Best Practices

1. **Regular Rotation**: Replace certificates before expiration
2. **Secure Storage**: Keep private keys secure (600 permissions)
3. **Monitoring**: Set up expiration alerts
4. **Backup**: Maintain secure backups of certificates
5. **Revocation**: Have a plan for certificate revocation if compromised

## Integration with CI/CD

For automated testing environments:

```yaml
# Example GitHub Actions step
- name: Setup SSL certificates
  run: |
    ./generate_certs.sh
    ./start_web_ssl.sh &
    sleep 5
    ./test_certs.sh
```

This setup provides a complete SSL/TLS testing environment suitable for development, testing, and demonstration purposes.