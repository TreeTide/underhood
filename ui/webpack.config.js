const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { VueLoaderPlugin } = require('vue-loader')
const webpack = require('webpack');

const config = {
  entry: "./src/main.js",
  output: {
          path: path.resolve(__dirname, 'dist'),
          filename: '[name].js'
  },
  resolve: {
    alias: {
      'vue$': 'vue/dist/vue.esm.js'
    }
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/index.html',
    }),
    new webpack.ProvidePlugin({
      $: 'jquery2',
      jQuery: 'jquery2',
      _: 'lodash',
    }),
    new VueLoaderPlugin(),
  ],
  optimization: {
    splitChunks: {
      chunks: 'all',  // 'async' in default
      minSize: 20000,
      minRemainingSize: 0,
      minChunks: 1,
      maxAsyncRequests: 30,
      maxInitialRequests: 30,
      enforceSizeThreshold: 50000,
      cacheGroups: {
        defaultVendors: {
          test: /[\\/]node_modules[\\/]/,
          priority: -10,
          name: 'vendors',
          reuseExistingChunk: true,
        },
        default: {
          minChunks: 2,
          priority: -20,
          name: 'main',
          reuseExistingChunk: true,
        },
      },
    },
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          // Seems to be auto-used by vue-loader?
          'vue-style-loader',
          {
            loader: "style-loader",
            options: {
              // to prevent some problem with normalize.css
              // TODO(cleanup): might remove after normalize.css bump
              esModule: false,
            },
          },
          'css-loader',
        ]
      },
      {
        test: /\.vue$/,
        loader: 'vue-loader'
      },
      //{
      //  test: /\.svg$/,
      //  loader: 'svg-inline-loader'
      //},
      {
        // See https://chriscourses.com/blog/loading-fonts-webpack
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        type: 'asset/resource',
      },
    ]
  },
  devServer: {
    static: path.join(__dirname, 'static'),
    compress: true,
    port: 9000,
    host: '0.0.0.0',
    // To bind to 0.0.0.0, see
    // https://github.com/webpack/webpack-dev-server/issues/882.
    allowedHosts: 'all',

    hot: true,
    client: {
      // Show compile errors in popup.
      overlay: true,
    },
    proxy: [
      { context: ['/api'],
        target: 'http://localhost:8081',
      },
    ],
  },
};

module.exports = config;
