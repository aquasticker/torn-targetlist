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
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            debug: false
                        }
                    }
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
    }
};
