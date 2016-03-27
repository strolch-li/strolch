/**
 * Created by eitch on 2016-03-27
 */

/*
 * Define the resources namespace
 */
if (typeof strolch.parts == 'undefined') {
    strolch.parts = {};
}
strolch.parts.model = {};

jQuery(document).ready(function ($) {
    strolch.parts.model.init();
});

strolch.parts.model.init = function (domParent) {

    $("#page-content").load("parts/model.html");

    strolch.parts.model.registerHandlers();
};

strolch.parts.model.registerHandlers = function () {


};

strolch.parts.model.start = function () {

};