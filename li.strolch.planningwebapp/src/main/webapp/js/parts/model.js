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

    // query        => the search criteria
    // queryBy      => the fields to use in searching, default is null/all
    // sortBy       => a single column to sort by
    // ascending    => true|false
    // pageSize     => integer, max number of elements to return per page,

    // prepare query data
    var data = {
        realmName: '',
        draw: 1,
        pageSize: 1,
        page: 0,
        query: '',
        queryBy: 'Id, Name',
        types: '',
        orderBy: '',
        ascending: true
    };

    var url = strolch.fn.url(strolch.const.urls.resources);
    var columns = strolch.parts.model.prepareColumns();
    strolch.fn.initDataTable('modelResourceTable', columns, url, data);
};

strolch.parts.model.prepareColumns = function () {
    var columns = [];
    columns.push({title: 'Id', width: 50, data: 'Id'});
    columns.push({title: 'Name', data: 'Name'});
    columns.push({title: 'Type', data: 'Type'});

    return columns;
};