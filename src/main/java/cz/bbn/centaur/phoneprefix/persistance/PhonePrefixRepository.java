package cz.bbn.cerberus.phoneprefix.persistance;

import org.springframework.data.jpa.repository.JpaRepository;

public interface PhonePrefixRepository extends JpaRepository<PhonePrefixEntity, String> {
}
