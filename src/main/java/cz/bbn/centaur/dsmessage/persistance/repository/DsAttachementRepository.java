package cz.bbn.cerberus.dsmessage.persistance.repository;

import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageAttachementEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface DsAttachementRepository extends JpaRepository<DsMessageAttachementEntity, Long> {

    @Query("select entity.file from DsMessageAttachementEntity entity where id = :id")
    byte[] getFileById(Long id);
}
