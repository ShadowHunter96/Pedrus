package cz.bbn.cerberus.document.documentfileservice;

import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

@Service
public class LocalFileService implements DocumentFileInterface {

    private final String path;

    public LocalFileService(AppEnv appEnv) {
        path = appEnv.getStringProperty(AppProperty.LOCAL_FILE_PATH);
    }

    @Override
    public void saveDocument(DocumentFileDto documentFileDto) throws IOException {
        File theDir = new File(path);
        if (!theDir.exists()) {
            theDir.mkdirs();
        }

        Path filePath = Paths.get(path.concat("//").concat(documentFileDto.getName()));

        Files.write(filePath, documentFileDto.getFileData().readAllBytes());
    }

    @Override
    public void updateName(String originalName, String newName) throws IOException {
        Path originalFilePath = Paths.get(path.concat("//").concat(originalName));
        Path newFilePath = Paths.get(path.concat("//").concat(newName));
        Files.move(originalFilePath, newFilePath, StandardCopyOption.REPLACE_EXISTING);
    }

    @Override
    public DocumentFileDto getFile(String name) throws SystemException, IOException {
        File file = new File(Paths.get(path.concat("//").concat(name)).toUri());
        if (file.exists()) {
            DocumentFileDto documentFileDto = new DocumentFileDto();
            documentFileDto.setName(name);
            InputStream inputStream = new ByteArrayInputStream(FileUtils.readFileToByteArray(file));
            documentFileDto.setFileData(inputStream);
            return documentFileDto;
        }
        return null;
    }

    @Override
    public void deleteFile(String name) throws SystemException {
        File file = new File(Paths.get(path.concat("//").concat(name)).toUri());
        file.deleteOnExit();
    }
}
