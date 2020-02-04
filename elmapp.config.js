module.exports = {
  configureWebpack: (config, _) => {
    config.module.rules.push({
      test: /\.scss$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: [
        { loader: 'style-loader' },
        { loader: 'css-loader' },
        {
          loader: 'sass-loader',
          options: {
            sassOptions: {
              includePaths: ['./node_modules'],
            },
          },
        },
      ],
    });

    return config;
  },
};
