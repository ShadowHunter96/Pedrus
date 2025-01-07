package cz.bbn.cerberus.document.persistance.repository;

import cz.bbn.cerberus.document.persistance.entity.DocumentFileEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface DocumentFileRepository extends JpaRepository<DocumentFileEntity, String> {

    @Modifying
    @Query(" update DocumentFileEntity entity " +
            "set entity.name = :newName " +
            "where entity.name = :originalName")
    void updateName(String originalName, String newName);
}
