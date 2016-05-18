var path = require("path");

var targetEnv = process.env.npm_lifecycle_event == 'build' ? 'production' : 'development';

var config = {
    entry: {
        app: ['./webpack/entry.js']
    },
    module: {
        loaders: [
            { test: /.html$/, exclude: /node_modules/, loader: 'file?name=[name].[ext]' },
            { test: /\.elm$/, exclude: [/elm-stuff/, /node_modules/], loader: 'elm-webpack' },
            { test: /\.css$/, loader: 'style!css' },
            { test: /\.(ttf|eot|svg|woff[0-9]*)$/, loader: 'url?limit=10000&name=assets/fonts/[name].[ext]?[hash]' },
            { test: /\.(png|jpg)$/, loader: 'url?limit=10000&name=assets/images/[name].[ext]?[hash]' },
            { test: /\.scss$/, loader: 'style!css!sass' }
        ],
        noParse: [/.elm$/]
    },
    output: {
        path: "./dist",
        filename: "index.js"
    },
    resolve: {
        extensions: ['', '.ts', '.elm', '.webpack.js', '.web.js', '.js', ".scss", ".css" ],
        alias: {
            "semantic.css" : path.join(__dirname,"node_modules/semantic-ui-css/semantic.css"),
            "toastr.css": path.join(__dirname, "node_modules/toastr/build/toastr.css")
        }
    },
    externals: {
      "fs" : undefined,
      "cpexcel": "cpexcel",
      "./cptable" : "cptable"
    },
    devServer: {
        inline: true, stats: "errors-only"
    }
};

var finalConfig;
if (targetEnv === "development"){
  console.log('in dev mode')
    finalConfig = config;
} else {
  console.log('not in dev mode')
    finalConfig = config;
    
}

module.exports = finalConfig;