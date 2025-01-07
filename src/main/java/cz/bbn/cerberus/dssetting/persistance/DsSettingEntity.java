package cz.bbn.cerberus.dssetting.persistance;

import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "ds_setting", schema = "security")
@Getter
@Setter
public class DsSettingEntity {

    @Id
    private String id;

    private String name;
    private String description;

    @Column(name = "login_by_certificate")
    private Boolean loginByCertificate;

    @Column(name = "login")
    private String login;

    @Column(name = "password")
    private String password;

    @Column(name = "certificate")
    private byte[] certificate;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    private Boolean deleted;
}
