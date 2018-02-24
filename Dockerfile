FROM abiosoft/caddy
COPY ./build /srv
COPY Caddyfile /etc/Caddyfile