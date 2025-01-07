package cz.bbn.cerberus.document.persistance.repository;

import cz.bbn.cerberus.document.persistance.entity.DocumentEntity;
import cz.bbn.cerberus.documenttype.persistance.DocumentTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface DocumentRepository
        extends JpaRepository<DocumentEntity, String>, JpaSpecificationExecutor<DocumentEntity> {

    @Query(" select entity.id " +
            "from DocumentEntity entity " +
            "where entity.documentTypeEntity.id = :id")
    List<String> getUsedDocumentTypeList(String id);

    @Modifying
    @Query(" update DocumentEntity entity " +
            "set entity.documentTypeEntity = :newType " +
            "where entity.documentTypeEntity = :oldType")
    void changeDocumentType(DocumentTypeEntity oldType, DocumentTypeEntity newType);

    DocumentEntity findByName(String name);
}
