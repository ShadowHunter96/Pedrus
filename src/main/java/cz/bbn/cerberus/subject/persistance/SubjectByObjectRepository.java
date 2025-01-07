package cz.bbn.cerberus.subject.persistance;

import org.springframework.data.jpa.repository.JpaRepository;

public interface SubjectByObjectRepository extends JpaRepository<SubjectByObjectEntity, SubjectObjectId> {
}
