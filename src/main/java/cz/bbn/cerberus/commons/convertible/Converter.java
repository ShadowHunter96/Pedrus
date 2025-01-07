package cz.bbn.cerberus.commons.convertible;


public interface Converter<DOMAIN, ENTITY> {

    //TODO probrat v tymu naming konvence (lepsi nazev tridy, lepsi nazev metody)
    DOMAIN fromEntities(ENTITY ent);

}