# GitHub Repository Sync System - Summary

## Overview

This system automatically synchronizes a GitHub repository's file structure to a local JSON file whenever changes are pushed to the main branch. It uses GitHub webhooks for real-time notifications and the GitHub API to fetch repository contents.

## Key Features

1. **Real-time Updates**: Receives push notifications via GitHub webhooks
2. **Security**: Verifies webhook signatures to ensure authenticity
3. **Efficient**: Fetches entire repository tree in a single API call
4. **Simple Storage**: Outputs a single JSON file with all repository metadata
5. **Extensible**: Modular design allows easy addition of features

## Architecture Summary

```
GitHub Push → Webhook → Verify → Fetch Tree → Generate JSON → Save File
```

## Quick Start

1. **Set Environment Variables**:
   ```bash
   export GITHUB_TOKEN="your-personal-access-token"
   export GITHUB_WEBHOOK_SECRET="your-webhook-secret"
   export GITHUB_REPO_OWNER="your-username"
   export GITHUB_REPO_NAME="your-repo"
   ```

2. **Configure GitHub Webhook**:
   - URL: `https://your-domain.com/webhook/github`
   - Secret: Same as `GITHUB_WEBHOOK_SECRET`
   - Events: Just the push event

3. **Run the Application**:
   ```bash
   stack build
   stack run
   ```

## Output Format

The system generates a JSON file at `./data/json/repository-data.json`:

```json
{
  "repository": {
    "name": "notes-repo",
    "owner": "username",
    "updated_at": "2025-01-16T18:00:00Z"
  },
  "files": [
    {
      "path": "docs/README.md",
      "name": "README.md",
      "size": 1234,
      "sha": "abc123...",
      "type": "file",
      "last_modified": "2025-01-16T17:00:00Z"
    }
  ],
  "total_files": 42,
  "generated_at": "2025-01-16T18:15:00Z"
}
```

## Module Structure

- **Core.Types**: Data type definitions
- **Core.Config**: Configuration management
- **Security.GitHub**: Webhook signature verification
- **Client.GitHub**: GitHub API client
- **Generator.Repository**: JSON generation
- **Utils.FileSystem**: File operations
- **Utils.Logger**: Logging utilities
- **Api.Webhook**: Webhook handler
- **Api.Router**: Route definitions

## Security Considerations

1. **Webhook Secret**: Use a strong, unique secret
2. **Personal Access Token**: Use minimal permissions (repo:read)
3. **HTTPS Only**: Always use HTTPS in production
4. **Signature Verification**: Never skip webhook verification

## Future Enhancements

Once the basic system is working, consider adding:

1. **Markdown Processing**: Parse markdown files for metadata
2. **Caching**: Cache GitHub API responses to reduce API calls
3. **Queue System**: Process webhooks asynchronously
4. **Multiple Repositories**: Support syncing multiple repos
5. **Incremental Updates**: Only fetch changed files
6. **Database Storage**: Store in database instead of JSON
7. **API Endpoints**: Expose the data via REST API
8. **WebSocket Updates**: Real-time updates to connected clients

## Development Workflow

1. **Local Testing**: Use ngrok to expose local server
2. **Logging**: Set `LOG_LEVEL=DEBUG` for detailed logs
3. **Manual Trigger**: Use GitHub's "Redeliver" webhook feature
4. **Unit Tests**: Test individual modules in isolation
5. **Integration Tests**: Test the complete flow

## Troubleshooting Checklist

- [ ] Webhook URL is HTTPS and accessible
- [ ] Webhook secret matches in both places
- [ ] GitHub token has repository access
- [ ] Output directory exists and is writable
- [ ] All environment variables are set
- [ ] Application logs show incoming requests
- [ ] GitHub webhook page shows successful deliveries

## Dependencies

The system requires these Haskell packages:
- `scotty`: Web framework
- `aeson`: JSON parsing/generation
- `http-client`: HTTP requests
- `github`: GitHub API client (optional)
- `cryptonite`: Cryptographic operations
- `bytestring`, `text`: String handling
- `directory`, `filepath`: File operations

## Next Steps

1. Review the architecture document for detailed design
2. Follow the implementation guide for step-by-step instructions
3. Use the code examples as templates for each module
4. Start with the Config and Logger modules
5. Implement and test each module incrementally
6. Deploy to a server with HTTPS support

## Questions to Consider

Before starting implementation:

1. **File Filtering**: Should we filter certain file types?
2. **Size Limits**: How to handle very large repositories?
3. **Error Recovery**: How to handle partial failures?
4. **Monitoring**: What metrics should we track?
5. **Backup**: Should we keep historical versions?