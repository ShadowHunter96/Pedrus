package cz.bbn.cerberus.contactperson.persistance.repository;

import cz.bbn.cerberus.contactperson.persistance.entity.ContactPersonSimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ContactPersonSimpleRepository extends JpaRepository<ContactPersonSimpleEntity, String> {
}
