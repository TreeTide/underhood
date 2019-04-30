const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const VueLoaderPlugin = require('vue-loader/lib/plugin')
const webpack = require('webpack');

const config = {
	entry: "./src/main.js",
	output: {
		path: path.resolve(__dirname, 'dist'),
		filename: 'bundle.js'
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
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          chunks: 'all'
        }
      },
    },
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          // Seems to be auto-used by vue-loader?
          //'vue-style-loader',
          'style-loader',
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
        use: [{
            loader: 'file-loader',
            options: {
                name: '[name].[ext]',
                outputPath: 'fonts/'
            }
        }]
      },
    ]
  },
  devServer: {
    contentBase: path.join(__dirname, 'static'),
    compress: true,
    port: 9000,
    host: '0.0.0.0',
    // To bind to 0.0.0.0, see
    // https://github.com/webpack/webpack-dev-server/issues/882.
    disableHostCheck: true,

    hot: true,
    watchOptions: {
      poll: true
    },
    // Show compile errors in popup.
    overlay: true,

    proxy: {
      '/api': 'http://localhost:8081',  // Frontend-server
    },
  },
};

module.exports = config;
