import setuptools
from setuptools import setup

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name='dalab2',
    version='0.0.1a0',
    description='A example Python package',
    long_description=long_description,
    url='https://github.com/Mizzza54/dalab2',
    author='Michael Gerasimov',
    author_email='gerasimov.misha2001@gmail.com',
    license='License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
    package_dir={"": "src"},
    packages=setuptools.find_packages(where="src"),
    python_requires=">=3.6",
    install_requires=[
        'numpy>=1.16',
        'matplotlib',
    ],

    classifiers=[
        'Development Status :: 1 - Planning',
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
    ],
)