var gulp = require('gulp');
var $ = require('gulp-load-plugins')();
var merge = require('merge-stream');
var del = require('del');

// temporary until gulp 4.0 is released
var runSequence = require('run-sequence');

var vulcanize = require('gulp-vulcanize');
var minify = require('gulp-minify');
var crisper = require('gulp-crisper');
var shell = require('gulp-shell');
var rename = require("gulp-rename");

// Copy All Files At The Root Level
gulp.task('copy', function () {

    var svg = gulp.src([
        'app/**/*.svg', '!app/bower_components/**/hero.svg', '!app/bower_components/mocha/**/*', '!app/bower_components/**/{demo,test,site}/**/*'
    ])
        .pipe(gulp.dest('www/'));

    var imgs = gulp.src([
        'app/img/*.svg', 'app/img/*.ico', 'app/img/*.png', 'app/img/*.gif'
    ])
        .pipe(gulp.dest('www/img/'));

    var json = gulp.src([
        'app/bower_components/strolch-wc-auth/locales.json'
    ])
        .pipe(gulp.dest('www/bower_components/strolch-wc-auth/'));

    var app_files = gulp.src([
        'app/bower.json', 'app/manifest.json', 'app/locales.json', '**/moment.min.js'
    ])
        .pipe(gulp.dest('www/'));

    var js = gulp.src([
        'src/**/*'
    ])
        .pipe(gulp.dest('js'));

    return merge(svg, imgs, json, app_files, js)
        .pipe($.size({title: 'Copy app files to dist dir:'}));
});

gulp.task('vulcanize', function () {
    return gulp.src('app/index.html')
        .pipe(vulcanize({
            stripComments: true,
            inlineScripts: true,
            inlineCss: true
        }))
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
