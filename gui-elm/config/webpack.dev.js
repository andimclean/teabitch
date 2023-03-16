const path = require('path');
const CopyWebpackPlugin = require("copy-webpack-plugin");
const { merge } = require('webpack-merge');
const common = require('./webpack.common.js');


const dev = {
    mode: 'development',
    plugins: [
        // Copy static assets
        new CopyWebpackPlugin({
            patterns: [{ from: "src/assets" }]
        })
    ],
    devServer: {
        hot: "only",
        client: {
            logging: "info"
        },
        static: { directory: path.join(__dirname, "../src/assets") },
        devMiddleware: {
            publicPath: "/",
            stats: "errors-only"
        },
        historyApiFallback: true,
        // feel free to delete this section if you don't need anything like this
        setupMiddlewares: (middlewares, devServer) => {
            // on port 3000
            devServer.app.get("/test", function (req, res) {
                res.json({ result: "You reached the dev server" });
            });
            return middlewares;
        }
    },
};

module.exports = env => {
    const withDebug = !env.nodebug;
    return merge(common(withDebug), dev);
};
