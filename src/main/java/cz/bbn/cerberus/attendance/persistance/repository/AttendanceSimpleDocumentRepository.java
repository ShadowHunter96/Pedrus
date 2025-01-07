package cz.bbn.cerberus.attendance.persistance.repository;

import cz.bbn.cerberus.attendance.persistance.entity.AttendanceSimpleDocumentEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface AttendanceSimpleDocumentRepository extends JpaRepository<AttendanceSimpleDocumentEntity, Long>, JpaSpecificationExecutor<AttendanceSimpleDocumentEntity> {

    @Modifying
    @Query("update AttendanceDocumentEntity entity set entity.deleted = true where name = :name")
    void updateDeletedByName(String name);
}
