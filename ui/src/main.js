import 'normalize.css';

import Vue from 'vue';
import VueRouter from 'vue-router';
import App from './App.vue'

window.onload = function() {
  Vue.use(VueRouter);

  const routes = [
    {
      path: '/',
      component: App },
    {
      path: '/file/:ticket/:line?',
      name: 'file',
      component: App,
      props: (route) => {
        let res = {
          ticket: route.params.ticket,
        };
        const line = Number.parseInt(route.params.line, 10);
        if (Number.isNaN(line)) {
          return res;
        }
        res.line = line;
        return res;
      },
    },
  ];

  const router = new VueRouter({
    routes
  });

  new Vue({
    router
  }).$mount('#app');
};
