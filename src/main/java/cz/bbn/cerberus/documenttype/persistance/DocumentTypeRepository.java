package cz.bbn.cerberus.documenttype.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface DocumentTypeRepository extends JpaRepository<DocumentTypeEntity, String> {

    @Query(" select entity " +
            "from DocumentTypeEntity entity " +
            "where entity.allowed = true and entity.id != :exceptId")
    List<DocumentTypeEntity> findAllAllowed(String exceptId);

    @Query( "select entity.allowed " +
            "from DocumentTypeEntity entity " +
            "where entity.id = :id")
    boolean isDocumentTypeAllowed(String id);
}
