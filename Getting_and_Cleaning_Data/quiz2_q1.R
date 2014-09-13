library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications
#    Insert your values below - if secret is omitted, it will look it up in
#    the GITHUB_CONSUMER_SECRET environmental variable.
#
#    Use http://localhost:1410 as the callback url
myapp <- oauth_app("github", key="911c8cd6eab984aa5fd8",
                   secret="2521e2bfa305fc84247037980120bb77f263afbd")
# myapp <- oauth_app("github", "911c8cd6eab984aa5fd8")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API

req <- GET("https://api.github.com/users/jtleek/repos",
           config(token = github_token))
stop_for_status(req)
content(req)