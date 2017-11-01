li.strolch.service
==================

Service API for Strolch


Running tests
------------------

Start PostgreSQL console and create the users:

	$ sudo -u postgres psql
	$ postgres=# 
	create user cacheduser with password 'test';
	create database cacheduserdb;
	GRANT ALL PRIVILEGES ON DATABASE cacheduserdb to cacheduser;
	GRANT CONNECT ON DATABASE cacheduserdb TO cacheduser;
	
	create user transactionaluser with password 'test';
	create database transactionaluserdb;
	GRANT ALL PRIVILEGES ON DATABASE transactionaluserdb to transactionaluser;
	GRANT CONNECT ON DATABASE transactionaluserdb TO transactionaluser;
	
	create user cacheduserauditsversioning with password 'test';
	create database cacheduserauditsversioningdb;
	GRANT ALL PRIVILEGES ON DATABASE cacheduserauditsversioningdb to cacheduserauditsversioning;
	GRANT CONNECT ON DATABASE cacheduserauditsversioningdb TO cacheduserauditsversioning;

You can revoke the privileges with the following:

	revoke ALL PRIVILEGES ON DATABASE cacheduserdb from cacheduser;
	drop user cacheduser;
	drop database cacheduserdb;
	
	revoke ALL PRIVILEGES ON DATABASE transactionaluserdb from transactionaluser;
	drop user transactionaluser;
	drop database transactionaluserdb;
