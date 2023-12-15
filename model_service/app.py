from flask import Flask
from PIL import Image
import pickle
import dnnlib
import numpy as np
import torch
import torch_utils
import base64
from io import BytesIO

app = Flask(__name__)

def image_to_base64(img):
    buf = BytesIO()
    img.save(buf, format="PNG")
    return base64.b64encode(buf.getvalue()).decode()


@app.route('/<int:seed>', methods=['GET'])
def generate_image(seed):
    with open('deepl_model.pkl', 'rb') as f:
        G = pickle.load(f)['G_ema'].cuda()
    z = torch.from_numpy(np.random.RandomState(seed).randn(1, G.z_dim)).cuda()
    c = None
    img = G(z, c)
    img = (img.permute(0, 2, 3, 1) * 127.5 + 128).clamp(0, 255).to(torch.uint8)
    img = Image.fromarray(img[0].cpu().numpy(), 'RGB')
    return image_to_base64(img)


if __name__ == "__main__":
    app.run(debug=True, host="0.0.0.0")
