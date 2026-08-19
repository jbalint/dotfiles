# Jira

- The Jira CLI is `jira`; the configured server is `https://jessandmorgan.com/jira`.
- Use the `ai-automation` account with bearer authentication.
- Read the PAT at runtime from `pass show 'Insight/N88-1033/personal-access-token'`; never print, log, or persist it.
- Invoke the CLI with `JIRA_AUTH_TYPE=bearer` and `JIRA_API_TOKEN` populated from `pass` for that command.
- Treat issue creation, edits, comments, transitions, assignments, worklogs, links, and deletes as write operations requiring explicit user intent.
- If the user does not provide labels when creating an issue, prefer reusing labels already used in the project (check recent issues for existing labels) over inventing new ones; ask for confirmation before applying them.
- Always include the direct Jira issue URL when an issue is created, found via search, viewed, or modified.
- Search across all Jira projects by default—for example, with `project IS NOT EMPTY`—unless the user specifies a project.
- After creating an issue, ask whether it should be added to the current sprint.
- Jira uses one shared sprint board (legacy rapid view ID `1`), but board discovery may return no boards. Use the legacy GreenHopper sprint picker at `/rest/greenhopper/1.0/sprint/picker` to identify active and future sprints; add issues with `jira sprint add <sprint-id> <issue-key>`.
- Comments are plain Jira **wiki markup** via the REST API — the `body` field is a String, not ADF. This is Jira Server/Data Center: an ADF `body` object is rejected with HTTP 400 ("can not deserialize `java.lang.String` out of `START_OBJECT`"), and a JSON-serialized ADF string renders as the raw JSON text. Use wiki markup instead: `h3.` headings, `*` bullets, `{{monospace}}`, `*bold*`, `_italic_`, and `||header||`/`|cell|` tables. The `jira issue comment add` CLI and `POST /rest/api/2/issue/{key}/comment` both render wiki markup. Use a Python script to build structured wiki markup; do not use invented pipe-table or markdown formatting in comments.
