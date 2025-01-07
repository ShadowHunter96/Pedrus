package cz.bbn.cerberus.document.documentfileservice;

import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import org.springframework.stereotype.Service;
import org.tmatesoft.svn.core.SVNCommitInfo;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNNodeKind;
import org.tmatesoft.svn.core.SVNProperties;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.fs.FSRepositoryFactory;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.io.diff.SVNDeltaGenerator;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

@Service
public class SvnFileService implements DocumentFileInterface {

    private final AppEnv appEnv;

    public SvnFileService(AppEnv appEnv) {
        this.appEnv = appEnv;
    }

    @Override
    public void saveDocument(DocumentFileDto documentFileDto) throws IOException, SVNException, SystemException {
        SVNRepository repository = getRepository();
        String filePath = appEnv.getSvnPath().concat("/").concat(documentFileDto.getName());
        boolean dirNotExists = repository.checkPath(appEnv.getSvnPath(), -1).equals(SVNNodeKind.NONE);
        boolean fileNoteExists = repository.checkPath(filePath, -1).equals(SVNNodeKind.NONE);
        if (fileNoteExists) {
            ISVNEditor editor = repository.getCommitEditor("file added ".concat(filePath), null);
            addFile(editor,
                    appEnv.getSvnPath(),
                    filePath,
                    documentFileDto.getFileData().readAllBytes(),
                    dirNotExists);
        } else {
            deleteFile(documentFileDto.getName());
            saveDocument(documentFileDto);
        }
    }

    @Override
    public void updateName(String originalName, String newName) throws IOException, SVNException, SystemException {
        DocumentFileDto documentFileDto = getFile(originalName);
        deleteFile(originalName);
        documentFileDto.setName(newName);
        saveDocument(documentFileDto);
    }

    @Override
    public DocumentFileDto getFile(String name) throws SystemException, IOException, SVNException {
        SVNProperties fileProperties = new SVNProperties();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        getRepository().getFile(appEnv.getSvnPath().concat("/").concat(name), -1, fileProperties, baos);
        DocumentFileDto documentFileDto = new DocumentFileDto();
        documentFileDto.setName(name);
        documentFileDto.setFileData(new ByteArrayInputStream(baos.toByteArray()));
        return documentFileDto;
    }

    @Override
    public void deleteFile(String name) throws SystemException, IOException, SVNException {
        SVNRepository repository = getRepository();
        String filePath = appEnv.getSvnPath().concat("/").concat(name);
        ISVNEditor editor = repository.getCommitEditor("delete file".concat(filePath), null);
        editor.openRoot(-1);
        editor.deleteEntry(filePath, -1);
        editor.closeEdit();
    }

    private SVNRepository getRepository() throws SVNException {
        FSRepositoryFactory.setup();
        SVNURL url = SVNURL.parseURIEncoded(appEnv.getSvnUrl());
        String userName = appEnv.getSvnUser();
        String userPassword = appEnv.getSvnPassword();
        SVNRepository repository = SVNRepositoryFactory.create(url);
        ISVNAuthenticationManager authManager = SVNWCUtil.createDefaultAuthenticationManager(
                userName, userPassword.toCharArray());
        repository.setAuthenticationManager(authManager);

        return repository;
    }

    private static SVNCommitInfo addFile(ISVNEditor editor, String dirPath,
                                         String filePath, byte[] data, boolean dirNotExists) throws SVNException {
        editor.openRoot(-1);
        if (dirNotExists) {
            editor.addDir(dirPath, null, -1);
        }
        editor.addFile(filePath, null, -1);
        editor.applyTextDelta(filePath, null);
        SVNDeltaGenerator deltaGenerator = new SVNDeltaGenerator();
        String checksum = deltaGenerator.sendDelta(filePath, new ByteArrayInputStream(data), editor, true);
        editor.closeFile(filePath, checksum);
        editor.closeDir();
        if (dirNotExists) {
            editor.closeDir();
        }

        return editor.closeEdit();
    }
}
