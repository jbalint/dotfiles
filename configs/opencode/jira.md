# Jira

- The Jira CLI is `jira`; the configured server is `https://jessandmorgan.com/jira`.
- Use the `ai-automation` account with bearer authentication.
- Read the PAT at runtime from `pass show 'Insight/N88-1033/personal-access-token'`; never print, log, or persist it.
- Invoke the CLI with `JIRA_AUTH_TYPE=bearer` and `JIRA_API_TOKEN` populated from `pass` for that command.
- Treat issue creation, edits, comments, transitions, assignments, worklogs, links, and deletes as write operations requiring explicit user intent.
- Always include the direct Jira issue URL when an issue is created, found via search, viewed, or modified.
- After creating an issue, ask whether it should be added to the current sprint.
