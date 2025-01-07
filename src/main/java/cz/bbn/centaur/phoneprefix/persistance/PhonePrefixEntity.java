package cz.bbn.cerberus.phoneprefix.persistance;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;


@Entity
@Table(name = "phone_prefix", schema = "enums")
@Getter
@Setter
public class PhonePrefixEntity {

    @Id
    @Column(name = "country_code")
    private String countryCode;

    @Column(name = "phone_prefix")
    private String phonePrefix;
}
