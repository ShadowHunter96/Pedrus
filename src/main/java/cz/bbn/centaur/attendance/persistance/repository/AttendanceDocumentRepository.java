package cz.bbn.cerberus.attendance.persistance.repository;

import cz.bbn.cerberus.attendance.persistance.entity.AttendanceDocumentEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AttendanceDocumentRepository extends JpaRepository<AttendanceDocumentEntity, Long> {
}
