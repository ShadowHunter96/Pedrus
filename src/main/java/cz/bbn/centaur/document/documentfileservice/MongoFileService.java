package cz.bbn.cerberus.document.documentfileservice;

import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.document.persistance.entity.DocumentFileMongoEntity;
import cz.bbn.cerberus.document.persistance.repository.DocumentFileMongoRepository;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.IOException;

@Service
public class MongoFileService implements DocumentFileInterface {

    private final DocumentFileMongoRepository documentFileMongoRepository;

    public MongoFileService(DocumentFileMongoRepository documentFileMongoRepository) {
        this.documentFileMongoRepository = documentFileMongoRepository;
    }


    @Override
    public void saveDocument(DocumentFileDto documentFileDto) throws IOException {
        DocumentFileMongoEntity entity = new DocumentFileMongoEntity();
        entity.setName(documentFileDto.getName());
        entity.setFile(documentFileDto.getFileData().readAllBytes());
        documentFileMongoRepository.save(entity);
    }

    @Override
    public void updateName(String originalName, String newName) throws IOException {
        DocumentFileMongoEntity entity = documentFileMongoRepository.findByName(originalName)
                .orElse(new DocumentFileMongoEntity());
        entity.setName(newName);
        documentFileMongoRepository.save(entity);

    }

    @Override
    public DocumentFileDto getFile(String name) throws SystemException, IOException {
        DocumentFileDto documentFileDto = new DocumentFileDto();
        documentFileDto.setName(name);

        DocumentFileMongoEntity entity = documentFileMongoRepository.findByName(name)
                .orElse(new DocumentFileMongoEntity());
        documentFileDto.setFileData(new ByteArrayInputStream(entity.getFile()));
        return documentFileDto;
    }

    @Override
    public void deleteFile(String name) throws SystemException, IOException {
        documentFileMongoRepository.deleteByName(name);
    }
}
