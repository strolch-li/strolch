//# sourceURL=model.js

/**
 * Created by eitch on 2016-03-27
 */

/*
 * Define the resources namespace
 */
if (typeof strolch.parts == 'undefined') {
    strolch.parts = {};
}

// define the namespace:
strolch.parts.model = {
    name: 'model'
};

// Required function init() - called to initialize when loaded
strolch.parts.model.init = function (domParent) {

    strolch.parts.model.prepareResourceTable();
    strolch.parts.model.prepareOrderTable();
    strolch.parts.model.prepareActivityTable();

    strolch.parts.model.registerHandlers();
};

// delegate function to register handlers (called from init)
strolch.parts.model.registerHandlers = function () {

    $('#part-model').find('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
        var tableSel = $(e.target).attr('href');
        $(tableSel).find('table').DataTable().draw();
    });

    console.log('Registered handlers');
};

// Required function show() - called when navigated to this method
strolch.parts.model.show = function () {
    $('#part-model').show();
};

// Required function hide() - called when navigated away
strolch.parts.model.hide = function () {
    $('#part-model').hide();
};

strolch.parts.model.prepareResourceTable = function () {

    // prepare query data
    var data = strolch.fn.dataTableDefaults();
    data.realmName = '';
    data.url = strolch.fn.url(strolch.const.urls.resources);
    data.tableId = 'modelResourceTable';
    data.searchFieldId = 'modelResourceTableSearch';

    var columns = [];
    columns.push({title: 'Id', width: 50, data: 'Id'});
    columns.push({title: 'Name', data: 'Name'});
    columns.push({title: 'Type', data: 'Type'});

    strolch.fn.initDataTable(data, columns);
};

strolch.parts.model.prepareOrderTable = function () {

    // prepare query data
    var data = strolch.fn.dataTableDefaults();
    data.realmName = '';
    data.url = strolch.fn.url(strolch.const.urls.orders);
    data.tableId = 'modelOrderTable';
    data.searchFieldId = 'modelOrderTableSearch';

    var columns = [];
    columns.push({title: 'Id', width: 50, data: 'Id'});
    columns.push({title: 'Name', data: 'Name'});
    columns.push({title: 'State', data: 'State'});
    columns.push({title: 'Date', data: 'Date'});
    columns.push({title: 'Type', data: 'Type'});

    strolch.fn.initDataTable(data, columns);
};

strolch.parts.model.prepareActivityTable = function () {

    // prepare query data
    var data = strolch.fn.dataTableDefaults();
    data.realmName = '';
    data.url = strolch.fn.url(strolch.const.urls.activities);
    data.tableId = 'modelActivityTable';
    data.searchFieldId = 'modelActivityTableSearch';

    var columns = [];
    columns.push({title: 'Id', width: 50, data: 'Id'});
    columns.push({title: 'Name', data: 'Name'});
    columns.push({title: 'Type', data: 'Type'});

    strolch.fn.initDataTable(data, columns);
};
