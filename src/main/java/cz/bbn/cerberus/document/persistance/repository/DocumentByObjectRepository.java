package cz.bbn.cerberus.document.persistance.repository;

import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectEntity;
import cz.bbn.cerberus.document.persistance.entity.DocumentByObjectId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface DocumentByObjectRepository extends JpaRepository<DocumentByObjectEntity, DocumentByObjectId> {

    List<DocumentByObjectEntity> findByIdDocumentName(String documentName);

    @Modifying
    void deleteByIdDocumentName(String newName);

    @Query("select entity.id.documentName from DocumentByObjectEntity entity " +
            "where entity.id.objectId = :objectId and entity.id.objectType = :objectType")
    List<String> findByObjectIdObjectType(String objectId, DocumentObjectEnum objectType);
}
