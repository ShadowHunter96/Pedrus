package cz.bbn.cerberus.commons.image;

import org.apache.commons.io.IOUtils;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletContext;
import java.io.IOException;
import java.io.InputStream;

@RestController
public class ImageController {

    private final ServletContext servletContext;

    public ImageController(ServletContext servletContext) {
        this.servletContext = servletContext;
    }

    @GetMapping(value = "/image-png/{png}" , produces = MediaType.IMAGE_PNG_VALUE)
    public @ResponseBody byte[] getPngImage(@PathVariable("png") String image) throws IOException {
        InputStream in = servletContext.getClass().getResourceAsStream("/static/img/".concat(image).concat(".png"));
        return IOUtils.toByteArray(in);
    }

    @GetMapping(value = "/image-gif/{gif}" , produces = MediaType.IMAGE_JPEG_VALUE)
    public @ResponseBody byte[] getGifImage(@PathVariable("gif") String image) throws IOException {
        InputStream in = servletContext.getClass().getResourceAsStream("/static/img/".concat(image).concat(".gif"));
        return IOUtils.toByteArray(in);
    }
}
