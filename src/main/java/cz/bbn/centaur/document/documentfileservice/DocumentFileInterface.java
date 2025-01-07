package cz.bbn.cerberus.document.documentfileservice;

import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import org.tmatesoft.svn.core.SVNException;

import java.io.IOException;

public interface DocumentFileInterface {

    void saveDocument(DocumentFileDto documentFileDto) throws IOException, SVNException, SystemException;

    void updateName(String originalName, String newName) throws IOException, SVNException, SystemException;

    DocumentFileDto getFile(String name) throws SystemException, IOException, SVNException;

    void deleteFile(String name) throws SystemException, IOException, SVNException;
}
