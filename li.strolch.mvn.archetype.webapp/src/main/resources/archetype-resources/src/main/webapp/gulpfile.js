const gulp = require('gulp');
const $ = require('gulp-load-plugins')();
const merge = require('merge-stream');
const del = require('del');

// temporary until gulp 4.0 is released
const runSequence = require('run-sequence');

const vulcanize = require('gulp-vulcanize');
const minify = require('gulp-minify');
const crisper = require('gulp-crisper');
const shell = require('gulp-shell');
const rename = require("gulp-rename");
const replace = require('gulp-replace');

// Copy All Files At The Root Level
gulp.task('copy', function () {

    var svg = gulp.src([
        'app/**/*.svg', '!app/bower_components/**/hero.svg', '!app/bower_components/mocha/**/*', '!app/bower_components/**/{demo,test,site}/**/*'
    ])
        .pipe(gulp.dest('www/'));

    var font = gulp.src([
        'app/font/*'
    ])
        .pipe(gulp.dest('www/font/'));

    var imgs = gulp.src([
        'app/img/*.svg', 'app/img/*.ico', 'app/img/*.png', 'app/img/*.gif'
    ])
        .pipe(gulp.dest('www/img/'));

    var locales = gulp.src([
        'app/bower_components/**/locales.json'
    ])
        .pipe(gulp.dest('www/bower_components/'));

    var app_files = gulp.src([
        'app/bower.json', 'app/manifest.json', 'app/locales.json', '**/moment.min.js'
    ])
        .pipe(gulp.dest('www/'));

    var js = gulp.src([
        'src/**/*'
    ])
        .pipe(gulp.dest('js'));

    return merge(svg, font, imgs, locales, app_files, js)
        .pipe($.size({title: 'Copy app files to dist dir:'}));
});

gulp.task('vulcanize', function () {
    return gulp.src('app/index.html')
        .pipe(vulcanize({
            stripComments: true,
            inlineScripts: true,
            inlineCss: true
        }))
        .pipe(replace('<link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/css?family=Roboto+Mono:400,700|Roboto:400,300,300italic,400italic,500,500italic,700,700italic" crossorigin="anonymous">', ''))
        .pipe(replace("font-family: 'Roboto', 'Noto', sans-serif;", "font-family: 'Ubuntu-local', sans-serif;"))
        .pipe(crisper())
        .pipe(gulp.dest('www/'));
});

gulp.task('compress', function () {
    return gulp.src('www/index.js')
        .pipe(minify({}))
        .pipe(gulp.dest('www/'))
});

// Clean Output Directory
gulp.task('clean-www', del.bind(null, ['www/']));

// Remove unneeded files
gulp.task('del-files', del.bind(null, ['www/index-min.js']));
gulp.task('move-indexjs-js', function () {
    return gulp.src('./www/index-min.js') //
        .pipe(rename('index.js'))
        .pipe(gulp.dest('./www/'));
});

// bower install
gulp.task('bower', shell.task('bower install'));

// Build Production Files, the Default Task
gulp.task('prepare', ['bower'], function (cb) {
    runSequence('clean-www', cb);
});

gulp.task('default', ['prepare'], function (cb) {
    runSequence('copy', 'vulcanize', 'compress', 'del-files', cb);
});
