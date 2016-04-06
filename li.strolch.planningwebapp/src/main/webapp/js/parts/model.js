/**
 * Created by eitch on 2016-03-27
 */

/*
 * Define the resources namespace
 */
if (typeof strolch.parts == 'undefined') {
    strolch.parts = {};
}
strolch.parts.model = {
    name: 'model',
    loadedResources: false
};

strolch.parts.model.init = function (domParent) {

    strolch.parts.model.prepareResourceTable();

    strolch.parts.model.registerHandlers();
};

strolch.parts.model.registerHandlers = function () {

    console.log('Registered handlers');
};

strolch.parts.model.show = function () {


    $('#part-model').show();
};

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

    var columns = strolch.parts.model.prepareColumns();
    strolch.fn.initDataTable(data, columns);
};

strolch.parts.model.prepareColumns = function () {
    var columns = [];
    columns.push({title: 'Id', width: 50, data: 'Id'});
    columns.push({title: 'Name', data: 'Name'});
    columns.push({title: 'Type', data: 'Type'});

    return columns;
};