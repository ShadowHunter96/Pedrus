package cz.bbn.cerberus.api;

import cz.bbn.cerberus.commons.exception.SystemException;
import org.springframework.http.ResponseEntity;

import java.io.IOException;

public interface ApiAction<T>
{
    ResponseEntity<T> doAction() throws SystemException, IOException;
}
