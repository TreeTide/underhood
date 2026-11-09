FROM nginx:mainline

COPY distrib/ui /usr/share/nginx/html
# Important to name it default.conf.template, to overwrite the default.conf (which would try to bind to port 80)
COPY docker/underhood-nginx/nginx.conf.template /etc/nginx/templates/default.conf.template
