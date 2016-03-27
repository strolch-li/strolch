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

strolch.parts.model.init = function (domParent) {


    strolch.parts.model.registerHandlers();
};

strolch.parts.model.registerHandlers = function () {

    console.log("Registered handlers");
};

strolch.parts.model.show = function () {

};