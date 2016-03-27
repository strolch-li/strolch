
// THIS IS A TEMPLATE - EXTEND FOR OWN PART

/*
 * Define the TEMPLATE_NAME namespace
 */
if (typeof strolch.parts == 'undefined') {
    strolch.parts = {};
}

// define the namespace:
strolch.parts.TEMPLATE_NAME = {};

// Required function init() - called to initialize when loaded
strolch.parts.TEMPLATE_NAME.init = function () {

	strolch.parts.TEMPLATE_NAME.registerHandlers();    
};

// delegate function to register handlers (called from init)
strolch.parts.TEMPLATE_NAME.registerHandlers = function () {

};

// Required function show() - called when navigated to this method
strolch.parts.TEMPLATE_NAME.show = function () {

};