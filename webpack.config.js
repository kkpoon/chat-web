const path = require("path");

module.exports = {
    entry: {
        main: "./src/Main.elm"
    },
    output: {
        filename: "[name].js",
        path: path.resolve(__dirname, "public/lib"),
        publicPath: "/lib/",
        library: "Elm"
    },
    module: {
        noParse: [/.elm$/],
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: {
                    loader: "elm-webpack-loader",
                    options: { debug: true, warn: true }
                }
            }
        ]
    },
    devServer: {
        contentBase: path.resolve(__dirname, "public"),
        compress: true,
        port: 3000
    }
};
