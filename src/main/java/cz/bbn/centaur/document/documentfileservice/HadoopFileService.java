package cz.bbn.cerberus.document.documentfileservice;

import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.ByteArrayInputStream;
import java.io.IOException;

//@Service
public class HadoopFileService implements DocumentFileInterface {

    private final FileSystem fileSystem;

    public HadoopFileService(FileSystem fileSystem) {
        this.fileSystem = fileSystem;
    }

    @Override
    public void saveDocument(DocumentFileDto documentFileDto) throws IOException {
        String fileName = documentFileDto.getName();
        Path newPath = new Path(fileName);

        FSDataOutputStream outputStream = null;
        try {
            outputStream = fileSystem.create(newPath);
            outputStream.write(documentFileDto.getFileData().readAllBytes());
        } finally {
            if (null != outputStream) {
                outputStream.close();
            }
        }
    }

    @Override
    public void updateName(String originalName, String newName) throws IOException {
        Path oldPath = new Path(originalName);
        Path newPath = new Path(newName);

        fileSystem.rename(oldPath, newPath);
    }

    @Override
    public DocumentFileDto getFile(String name) throws SystemException, IOException {
        Path src = new Path(name);
        byte[] result;
        FSDataInputStream inputStream = null;
        try {
            inputStream = fileSystem.open(src);
            result = IOUtils.readFullyToByteArray(inputStream);
        } finally {
            if (null != inputStream) {
                inputStream.close();
            }
        }
        DocumentFileDto documentFileDto = new DocumentFileDto();
        documentFileDto.setName(name);
        documentFileDto.setFileData(new ByteArrayInputStream(result));
        return documentFileDto;
    }

    @Override
    public void deleteFile(String name) throws SystemException, IOException {
        Path src = new Path(name);
        fileSystem.deleteOnExit(src);
    }
}
