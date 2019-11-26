const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
    entry: ["./src/index.js"],
    output: {
        filename: "bundle.js",
        publicPath: "/",
        path: __dirname + "/dist",
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: 'Haskell • Glazier • React • TodoMVC',
            favicon: './public/favicon.ico',
            hash: true,
            template: './src/index.html',
            filename: 'index.html' //relative to root of the application
        })
    ],
    devServer: {
        writeToDisk: true,
    },
    module: {
        rules: [{
            test: /\.css$/,
            use: [
                'style-loader',
                'css-loader'
            ]
        }]
    }
};
