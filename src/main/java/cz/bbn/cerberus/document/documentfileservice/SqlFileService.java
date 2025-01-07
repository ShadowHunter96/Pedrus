package cz.bbn.cerberus.document.documentfileservice;

import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.document.factory.DocumentFileFactory;
import cz.bbn.cerberus.document.persistance.entity.DocumentFileEntity;
import cz.bbn.cerberus.document.persistance.repository.DocumentFileRepository;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.io.IOException;

@Service
public class SqlFileService implements DocumentFileInterface {

    private final DocumentFileRepository documentFileRepository;

    public SqlFileService(DocumentFileRepository documentFileRepository) {
        this.documentFileRepository = documentFileRepository;
    }

    @Override
    @Transactional
    public void saveDocument(DocumentFileDto documentFileDto) throws IOException {
        DocumentFileEntity documentEntity = DocumentFileFactory.fromDto(documentFileDto);
        documentFileRepository.save(documentEntity);
    }

    @Override
    public void updateName(String originalName, String newName) {
        documentFileRepository.updateName(originalName, newName);
    }


    @Override
    public DocumentFileDto getFile(String name) throws SystemException, IOException {
        return DocumentFileFactory.fromEntity(
                documentFileRepository.findById(name).orElse(new DocumentFileEntity()));
    }

    @Override
    @Transactional
    public void deleteFile(String name) throws SystemException {
        if (documentFileRepository.existsById(name)) {
            throw new SystemException(ErrorCode.DOCUMENT_FILE_NOT_EXISTS, name);
        }
        documentFileRepository.deleteById(name);
    }
}
