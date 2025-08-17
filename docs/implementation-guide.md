# GitHub Sync Implementation Guide

## Step 1: GitHub Webhook Setup

### In Your GitHub Repository:

1. Go to Settings → Webhooks → Add webhook
2. Configure the webhook:
   - **Payload URL:** `https://your-domain.com/webhook/github`
   - **Content type:** `application/json`
   - **Secret:** Generate a strong secret (e.g., `openssl rand -hex 32`)
   - **Events:** Select "Just the push event"
   - **Active:** Check the box

### Webhook Payload Example:

```json
{
  "ref": "refs/heads/main",
  "repository": {
    "name": "notes-repo",
    "full_name": "owner/notes-repo",
    "private": true
  },
  "pusher": {
    "name": "username",
    "email": "user@example.com"
  },
  "commits": [...]
}
```

## Step 2: Environment Configuration

Create a `.env` file (don't commit this!):

```bash
# GitHub Configuration
GITHUB_TOKEN=ghp_xxxxxxxxxxxxxxxxxxxx
GITHUB_WEBHOOK_SECRET=your-webhook-secret-here
GITHUB_REPO_OWNER=your-github-username
GITHUB_REPO_NAME=your-notes-repo

# Application Configuration
JSON_OUTPUT_PATH=./data/json/
LOG_LEVEL=INFO
PORT=3000
```

## Step 3: Implementation Order

### Phase 1: Core Infrastructure
1. **Config Module** - Load environment variables
2. **Logger Module** - Set up logging
3. **Types Module** - Define data types

### Phase 2: GitHub Integration
4. **GitHub Client** - API communication
5. **Security Module** - Webhook verification

### Phase 3: Business Logic
6. **Webhook Handler** - Process webhook events
7. **JSON Generator** - Create output files
8. **File System Utils** - Save files safely

### Phase 4: Integration
9. **Router Updates** - Add webhook route
10. **Main.hs Updates** - Initialize components

## Step 4: Key Implementation Details

### Webhook Signature Verification

GitHub signs webhooks using HMAC-SHA256. Here's the verification process:

1. Get the signature from `X-Hub-Signature-256` header
2. Compute HMAC-SHA256 of the request body using your secret
3. Compare the signatures (use constant-time comparison)

### GitHub API Usage

When fetching repository contents:

1. Use the `/repos/{owner}/{repo}/git/trees/{sha}?recursive=1` endpoint
2. This returns all files in one request (more efficient)
3. Handle rate limiting (5000 requests/hour with auth)

### JSON Generation Strategy

1. Fetch the repository tree
2. Filter for files only (ignore directories)
3. Group files by directory for better organization
4. Include metadata like size, path, and last modified date

### Atomic File Writing

To prevent corruption:

1. Write to a temporary file first
2. Ensure all data is flushed to disk
3. Atomically rename to final destination
4. Clean up on failure

## Step 5: Error Handling Patterns

### Webhook Errors

```haskell
-- Return appropriate HTTP status codes
400 Bad Request     - Malformed payload
401 Unauthorized    - Invalid signature
422 Unprocessable   - Not a push to main
500 Server Error    - Internal failures
```

### GitHub API Errors

- **401**: Invalid token → Check GITHUB_TOKEN
- **404**: Repository not found → Check owner/name
- **403**: Rate limited → Implement backoff
- **5xx**: GitHub down → Retry with exponential backoff

## Step 6: Testing Locally

### Using ngrok for Local Development

1. Install ngrok: `brew install ngrok`
2. Start your backend: `stack run`
3. Expose locally: `ngrok http 3000`
4. Use the HTTPS URL for GitHub webhook

### Test Webhook Delivery

1. Make a test commit to your repository
2. Check GitHub webhook settings for delivery status
3. View request/response details
4. Check your application logs

### Manual Testing with curl

```bash
# Test webhook endpoint (without signature verification)
curl -X POST http://localhost:3000/webhook/github \
  -H "Content-Type: application/json" \
  -H "X-GitHub-Event: push" \
  -d '{
    "ref": "refs/heads/main",
    "repository": {
      "name": "test-repo",
      "full_name": "owner/test-repo"
    }
  }'
```

## Step 7: Production Deployment

### Pre-deployment Checklist

- [ ] All environment variables set
- [ ] HTTPS configured (Let's Encrypt recommended)
- [ ] Webhook secret is strong and unique
- [ ] GitHub token has minimal permissions
- [ ] Output directory has write permissions
- [ ] Logging configured appropriately
- [ ] Error alerting set up

### Nginx Configuration Example

```nginx
server {
    listen 443 ssl http2;
    server_name your-domain.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location /webhook/github {
        proxy_pass http://localhost:3000;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Important for webhook body verification
        proxy_set_header Content-Length $content_length;
        proxy_pass_request_body on;
    }
}
```

### Monitoring

1. **Application Logs**: Monitor for webhook processing errors
2. **GitHub Webhook Page**: Check recent deliveries
3. **File System**: Verify JSON files are being updated
4. **Metrics**: Track webhook processing time

## Step 8: Troubleshooting

### Common Issues

1. **Signature Verification Fails**
   - Check webhook secret matches
   - Ensure request body is not modified by proxy
   - Verify Content-Type header

2. **GitHub API 404**
   - Verify repository name and owner
   - Check if token has access to private repo
   - Ensure branch name is correct

3. **No JSON Output**
   - Check file permissions
   - Verify output directory exists
   - Look for errors in logs

4. **Webhook Not Received**
   - Verify HTTPS is working
   - Check firewall rules
   - Test with ngrok first

### Debug Mode

Set `LOG_LEVEL=DEBUG` to see:
- Raw webhook payloads
- GitHub API requests/responses
- File system operations
- Signature verification details

## Next Steps

After basic implementation:

1. **Add Caching**: Cache GitHub API responses
2. **Queue System**: Process webhooks asynchronously
3. **Metrics**: Add Prometheus metrics
4. **Health Checks**: Add `/health` endpoint
5. **Backup**: Implement JSON file versioning