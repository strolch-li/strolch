/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.fileserver;

import java.rmi.RemoteException;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public interface FileClient {

	/**
	 * Remote method with which a client can push parts of files to the server. It is up to the client to send as many
	 * parts as needed, the server will write the parts to the associated file
	 *
	 * @param filePart
	 * 		the part of the file
	 *
	 * @throws RemoteException
	 * 		if something goes wrong with the remote call
	 */
	void uploadFilePart(FilePart filePart) throws RemoteException;

	/**
	 * Remote method with which a client can delete files from the server. It only deletes single files if they exist
	 *
	 * @param fileDeletion
	 * 		the {@link FileDeletion} defining the deletion request
	 *
	 * @return true if the file was deleted, false if the file did not exist
	 *
	 * @throws RemoteException
	 * 		if something goes wrong with the remote call
	 */
	boolean deleteFile(FileDeletion fileDeletion) throws RemoteException;

	/**
	 * Remote method which a client can request part of a file. The server will fill the given {@link FilePart} with a
	 * byte array of the file, with bytes from the file, respecting the desired offset. It is up to the client to call
	 * this method multiple times for the entire file. It is a decision of the concrete implementation how much data is
	 * returned in each part, the client may pass a request, but this is not definitive
	 *
	 * @param filePart
	 * 		the part of the file
	 *
	 * @return the same file part, yet with the part of the file requested as a byte array
	 *
	 * @throws RemoteException
	 * 		if something goes wrong with the remote call
	 */
	FilePart requestFile(FilePart filePart) throws RemoteException;
}
