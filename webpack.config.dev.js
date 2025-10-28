const path = require('path');

module.exports = {
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [
                    /elm-stuff/,
                    /node_modules/
                ],

                use: [
                    {
                        loader: 'elm-hot-webpack-loader'
                    },
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            debug: true
                        }
                    },
                ]
            },
            {
                test: /\.css$/,
                use: [
                    {
                        loader: 'css-loader',
                        options: {
                          exportType: "string",
                        }
                    }
                ]
            }

        ]
    },

    output: {
        filename: 'js/main.js',
        path: path.resolve(__dirname, 'www'),
    },

    devServer: {
        static: path.join(__dirname, "www"),
        allowedHosts: 'all',
        headers: {
            'Access-Control-Allow-Origin': '*',
            'Access-Control-Allow-Headers': 'Origin, X-Requested-With, Content-Type, Accept'
        },
        host: 'localhost',
        client: {
            webSocketURL: {
               hostname: 'localhost',
               pathname: '/ws',
               password: 'dev-server',
               port: 8080,
               protocol: 'ws',
               username: 'webpack'
            }
       }
    }
};
